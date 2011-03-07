//
//  Majordomo Protocol broker
//  A minimal implementation of http://rfc.zeromq.org/spec:7 and spec:8
//
#include "zmsg.h"
#include "zlist.h"
#include "zhash.h"
#include "mdp.h"

//  We'd normally pull these from config data

#define HEARTBEAT_LIVENESS  3       //  3-5 is reasonable
#define HEARTBEAT_INTERVAL  2500    //  msecs
#define HEARTBEAT_EXPIRY    HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS
#define VERBOSE_BROKER      1       //  Trace activity?

//  This defines a single broker
typedef struct {
    void *context;              //  0MQ context
    void *socket;               //  Socket for clients & workers
    int verbose;                //  Print activity to stdout
    char *endpoint;             //  Broker binds to this endpoint
    zhash_t *services;          //  Hash of known services
    zhash_t *workers;           //  Hash of known workers
    zlist_t *waiting;           //  List of waiting workers
    uint64_t heartbeat_at;      //  When to send HEARTBEAT
} broker_t;

//  This defines a single service
typedef struct {
    char *name;                 //  Service name
    zlist_t *requests;          //  List of client requests
    zlist_t *waiting;           //  List of waiting workers
    size_t workers;             //  How many workers we have
} service_t;

//  This defines one worker, idle or active
typedef struct {
    char *identity;             //  Address of worker
    service_t *service;         //  Owning service, if known
    int64_t expiry;             //  Expires at unless heartbeat
} worker_t;


//  --------------------------------------------------------------------------
//  Broker functions
static broker_t *
    s_broker_new (int verbose);
static void
    s_broker_destroy (broker_t **self_p);
static void
    s_broker_bind (broker_t *self, char *endpoint);
static void
    s_broker_purge_workers (broker_t *self);

//  Service functions
static service_t *
    s_service_require (broker_t *self, char *name);
static void
    s_service_destroy (void *argument);
static void
    s_service_dispatch (broker_t *self, service_t *service, zmsg_t *msg);
static void
    s_service_internal (broker_t *self, char *service_name, zmsg_t *msg);

//  Worker functions
static worker_t *
    s_worker_require (broker_t *self, char *identity);
static void
    s_worker_delete (broker_t *self, worker_t *worker, int disconnect);
static void
    s_worker_destroy (void *argument);
static void
    s_worker_process (broker_t *self, char *sender, zmsg_t *msg);
static void
    s_worker_send (broker_t *self, worker_t *worker, char *command,
                   char *option, zmsg_t *msg);
static void
    s_worker_waiting (broker_t *self, worker_t *worker);
static int
    s_worker_expired (worker_t *worker);

//  Client functions
static void
    s_client_process (broker_t *self, char *sender, zmsg_t *msg);


//  ----------------------------------------------------------------------
//  Main broker work happens here

int main (void)
{
    s_version_assert (2, 1);
    s_catch_signals ();
    broker_t *self = s_broker_new (VERBOSE_BROKER);
    s_broker_bind (self, "tcp://*:5555");

    //  Get and process messages forever or until interrupted
    while (!s_interrupted) {
        zmq_pollitem_t items [] = { { self->socket,  0, ZMQ_POLLIN, 0 } };
        zmq_poll (items, 1, HEARTBEAT_INTERVAL * 1000);

        //  Process next input message, if any
        if (items [0].revents & ZMQ_POLLIN) {
            zmsg_t *msg = zmsg_recv (self->socket);
            if (self->verbose) {
                s_console ("I: received message:");
                zmsg_dump (msg);
            }
            char *sender = zmsg_pop (msg);
            char *empty  = zmsg_pop (msg);
            char *header = zmsg_pop (msg);

            if (strcmp (header, MDPC_CLIENT) == 0)
                s_client_process (self, sender, msg);
            else
            if (strcmp (header, MDPW_WORKER) == 0)
                s_worker_process (self, sender, msg);
            else {
                s_console ("E: invalid message:");
                zmsg_dump (msg);
                zmsg_destroy (&msg);
            }
            free (sender);
            free (empty);
            free (header);
        }
        //  Disconnect and delete any expired workers
        //  Send heartbeats to idle workers if needed
        if (s_clock () > self->heartbeat_at) {
            s_broker_purge_workers (self);
            worker_t *worker = zlist_first (self->waiting);
            while (worker) {
                s_worker_send (self, worker, MDPW_HEARTBEAT, NULL, NULL);
                worker = zlist_next (self->waiting);
            }
            self->heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;
        }
    }
    if (s_interrupted)
        printf ("W: interrupt received, shutting down...\n");

    s_broker_destroy (&self);
    return 0;
}


//  --------------------------------------------------------------------------
//  Constructor for broker object

static broker_t *
s_broker_new (int verbose)
{
    broker_t *self = (broker_t *) calloc (1, sizeof (broker_t));

    //  Initialize broker state
    self->context = zmq_init (1);
    self->socket = zmq_socket (self->context, ZMQ_XREP);
    self->verbose = verbose;
    self->services = zhash_new ();
    self->workers = zhash_new ();
    self->waiting = zlist_new ();
    self->heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;
    return (self);
}

//  --------------------------------------------------------------------------
//  Destructor for broker object

static void
s_broker_destroy (broker_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        broker_t *self = *self_p;
        zmq_close (self->socket);
        zmq_term (self->context);
        zhash_destroy (&self->services);
        zhash_destroy (&self->workers);
        zlist_destroy (&self->waiting);
        free (self);
        *self_p = NULL;
    }
}

//  --------------------------------------------------------------------------
//  Bind broker to endpoint, can call this multiple times
//  We use a single socket for both clients and workers.

void
s_broker_bind (broker_t *self, char *endpoint)
{
    zmq_bind (self->socket, endpoint);
    s_console ("I: MDP broker/0.1.1 is active at %s", endpoint);
}

//  ----------------------------------------------------------------------
//  Delete any idle workers that haven't pinged us in a while. Workers
//  are oldest to most recent, so we stop at the first alive worker.

static void
s_broker_purge_workers (broker_t *self)
{
    worker_t *worker = zlist_first (self->waiting);
    while (worker) {
        if (!s_worker_expired (worker))
            break;              //  Worker is alive, we're done here
        if (self->verbose)
            s_console ("I: deleting expired worker: %s", worker->identity);

        s_worker_delete (self, worker, 0);
        worker = zlist_first (self->waiting);
    }
}

//  ----------------------------------------------------------------------
//  Locate or create new service entry

static service_t *
s_service_require (broker_t *self, char *name)
{
    assert (name);
    service_t *service = zhash_lookup (self->services, name);
    if (service == NULL) {
        service = (service_t *) calloc (1, sizeof (service_t));
        service->name = strdup (name);
        service->requests = zlist_new ();
        service->waiting = zlist_new ();
        zhash_insert (self->services, name, service);
        zhash_freefn (self->services, name, s_service_destroy);
        if (self->verbose) {
            s_console ("I: received message:");
            s_console ("I: registering new service: %s", name);
        }
    }
    return service;
}

//  ----------------------------------------------------------------------
//  Destroy service object, called when service is removed from
//  broker->services.

static void
s_service_destroy (void *argument)
{
    service_t *service = argument;
    //  Destroy all queued requests
    while (zlist_size (service->requests)) {
        zmsg_t *msg = zlist_pop (service->requests);
        zmsg_destroy (&msg);
    }
    zlist_destroy (&service->requests);
    zlist_destroy (&service->waiting);
    free (service->name);
    free (service);
}

//  ----------------------------------------------------------------------
//  Dispatch requests to waiting workers as possible

static void
s_service_dispatch (broker_t *self, service_t *service, zmsg_t *msg)
{
    assert (service);
    if (msg)                    //  Queue message if any
        zlist_append (service->requests, msg);

    s_broker_purge_workers (self);
    while (zlist_size (service->waiting)
        && zlist_size (service->requests))
    {
        worker_t *worker = zlist_pop (service->waiting);
        zlist_remove (self->waiting, worker);
        zmsg_t *msg = zlist_pop (service->requests);
        s_worker_send (self, worker, MDPW_REQUEST, NULL, msg);
        zmsg_destroy (&msg);
    }
}

//  ----------------------------------------------------------------------
//  Handle internal service according to 8/MMI specification

static void
s_service_internal (broker_t *self, char *service_name, zmsg_t *msg)
{
    if (strcmp (service_name, "mmi.service") == 0) {
        service_t *service = zhash_lookup (self->services, zmsg_body (msg));
        if (service && service->workers)
            zmsg_body_set (msg, "200");
        else
            zmsg_body_set (msg, "404");
    }
    else
        zmsg_body_set (msg, "501");

    //  Remove & save client return envelope and insert the
    //  protocol header and service name, then rewrap envelope.
    char *client = zmsg_unwrap (msg);
    zmsg_wrap (msg, MDPC_CLIENT, service_name);
    zmsg_wrap (msg, client, "");
    free (client);
    zmsg_send (&msg, self->socket);
}

//  ----------------------------------------------------------------------
//  Creates worker if necessary

static worker_t *
s_worker_require (broker_t *self, char *identity)
{
    assert (identity);

    //  self->workers is keyed off worker identity
    worker_t *worker = zhash_lookup (self->workers, identity);
    if (worker == NULL) {
        worker = (worker_t *) calloc (1, sizeof (worker_t));
        worker->identity = strdup (identity);
        zhash_insert (self->workers, identity, worker);
        zhash_freefn (self->workers, identity, s_worker_destroy);
        if (self->verbose)
            s_console ("I: registering new worker: %s", identity);
    }
    return worker;
}

//  ----------------------------------------------------------------------
//  Deletes worker from all data structures, and destroys worker

static void
s_worker_delete (broker_t *self, worker_t *worker, int disconnect)
{
    assert (worker);
    if (disconnect)
        s_worker_send (self, worker, MDPW_DISCONNECT, NULL, NULL);

    if (worker->service) {
        zlist_remove (worker->service->waiting, worker);
        worker->service->workers--;
    }
    zlist_remove (self->waiting, worker);
    //  This implicitly calls s_worker_destroy
    zhash_delete (self->workers, worker->identity);
}

//  ----------------------------------------------------------------------
//  Destroy worker object, called when worker is removed from
//  broker->workers.

static void
s_worker_destroy (void *argument)
{
    worker_t *worker = argument;
    if (worker->identity)
        free (worker->identity);
    free (worker);
}

//  ----------------------------------------------------------------------
//  Process message sent to us by a worker

static void
s_worker_process (broker_t *self, char *sender, zmsg_t *msg)
{
    assert (zmsg_parts (msg) >= 1);     //  At least, command

    char *command = zmsg_pop (msg);
    int worker_ready = (zhash_lookup (self->workers, sender) != NULL);
    worker_t *worker = s_worker_require (self, sender);

    if (strcmp (command, MDPW_READY) == 0) {
        if (worker_ready)               //  Not first command in session
            s_worker_delete (self, worker, 1);
        else
        if (strlen (sender) >= 4  //  Reserved service name
        &&  memcmp (sender, "mmi.", 4) == 0)
            s_worker_delete (self, worker, 1);
        else {
            //  Attach worker to service and mark as idle
            char *service_name = zmsg_pop (msg);
            worker->service = s_service_require (self, service_name);
            worker->service->workers++;
            s_worker_waiting (self, worker);
            free (service_name);
        }
    }
    else
    if (strcmp (command, MDPW_REPLY) == 0) {
        if (worker_ready) {
            //  Remove & save client return envelope and insert the
            //  protocol header and service name, then rewrap envelope.
            char *client = zmsg_unwrap (msg);
            zmsg_wrap (msg, MDPC_CLIENT, worker->service->name);
            zmsg_wrap (msg, client, "");
            free (client);
            zmsg_send (&msg, self->socket);
            s_worker_waiting (self, worker);
        }
        else
            s_worker_delete (self, worker, 1);
    }
    else
    if (strcmp (command, MDPW_HEARTBEAT) == 0) {
        if (worker_ready)
            worker->expiry = s_clock () + HEARTBEAT_EXPIRY;
        else
            s_worker_delete (self, worker, 1);
    }
    else
    if (strcmp (command, MDPW_DISCONNECT) == 0)
        s_worker_delete (self, worker, 0);
    else {
        s_console ("E: invalid input message (%d)", (int) *command);
        zmsg_dump (msg);
    }
    free (command);
    zmsg_destroy (&msg);
}

//  --------------------------------------------------------------------------
//  Send message to worker
//  If pointer to message is provided, sends & destroys that message

static void
s_worker_send (
    broker_t *self, worker_t *worker,
    char *command, char *option, zmsg_t *msg)
{
    msg = msg? zmsg_dup (msg): zmsg_new (NULL);

    //  Stack protocol envelope to start of message
    if (option)                 //  Optional frame after command
        zmsg_push (msg, option);
    zmsg_push (msg, command);
    zmsg_push (msg, MDPW_WORKER);
    //  Stack routing envelope to start of message
    zmsg_wrap (msg, worker->identity, "");

    if (self->verbose) {
        s_console ("I: sending %s to worker",
            mdps_commands [(int) *command]);
        zmsg_dump (msg);
    }
    zmsg_send (&msg, self->socket);
}

//  ----------------------------------------------------------------------
//  This worker is now waiting for work

static void
s_worker_waiting (broker_t *self, worker_t *worker)
{
    //  Queue to broker and service waiting lists
    zlist_append (self->waiting, (void *) worker);
    zlist_append (worker->service->waiting, (void *) worker);
    worker->expiry = s_clock () + HEARTBEAT_EXPIRY;
    s_service_dispatch (self, worker->service, NULL);
}

//  ----------------------------------------------------------------------
//  Return 1 if worker has expired and must be deleted

static int
s_worker_expired (worker_t *worker)
{
    return worker->expiry < s_clock ();
}

//  ----------------------------------------------------------------------
//  Process a request coming from a client

static void
s_client_process (broker_t *self, char *sender, zmsg_t *msg)
{
    assert (zmsg_parts (msg) >= 2);     //  Service name + body

    char *service_name = zmsg_pop (msg);
    service_t *service = s_service_require (self, service_name);
    //  Set reply return address to client sender
    zmsg_wrap (msg, sender, "");
    if (strlen (service_name) >= 4
    &&  memcmp (service_name, "mmi.", 4) == 0)
        s_service_internal (self, service_name, msg);
    else
        s_service_dispatch (self, service, msg);
    free (service_name);
}
