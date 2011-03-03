//
//  Majordomo Protocol broker
//  A minimal implementation
//
#include "zmsg.h"
#include "zlist.h"
#include "zhash.h"
#include "mdp.h"

//  We'd normally pull these from config data

#define HEARTBEAT_LIVENESS  3       //  3-5 is reasonable
#define HEARTBEAT_INTERVAL  5000    //  msecs
#define HEARTBEAT_EXPIRY    HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS
#define VERBOSE_BROKER      0       //  Trace activity?

//  This defines a single broker
typedef struct {
    void *context;              //  0MQ context
    void *broker;               //  Socket for clients & workers
    int verbose;                //  Print activity to stdout
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
} service_t;

//  This defines one worker, idle or active
typedef struct {
    char *identity;             //  Address of worker
    service_t *service;         //  Owning service, if known
    int64_t expiry;             //  Expires at unless heartbeat
} worker_t;


//  --------------------------------------------------------------------------
//  Send message to worker
//  If pointer to message is provided, sends & destroys that message

static void
s_send_to_worker (
    broker_t *self, worker_t *worker,
    char *command, char *option, zmsg_t *msg)
{
    msg = msg? zmsg_dup (msg): zmsg_new (NULL);

    //  Stack protocol envelope to start of message
    if (option)                 //  Optional frame after command
        zmsg_push (msg, option);
    zmsg_push (msg, command);
    zmsg_push (msg, MDPS_WORKER);
    //  Stack routing envelope to start of message
    zmsg_push (msg, worker->identity);

    if (self->verbose) {
        s_console ("I: sending %s to worker",
            mdps_commands [(int) *command]);
        zmsg_dump (msg);
    }
    zmsg_send (&msg, self->broker);
}

//  Locate or create new service entry, free name when done
static service_t *
s_require_service (broker_t *self, char *name)
{
    assert (name);
    service_t *service = zhash_lookup (self->services, name);
    if (service == NULL) {
        service = (service_t *) calloc (1, sizeof (service_t));
        service->name = strdup (name);
        service->requests = zlist_new ();
        service->waiting = zlist_new ();
        zhash_insert (self->services, name, service);
        if (self->verbose) {
            s_console ("I: received message:");
            s_console ("I: registering new service: %s", name);
        }
    }
    free (name);                //  It's always a popped frame
    return service;
}

//  ----------------------------------------------------------------------
//  Creates worker if necessary

static worker_t *
s_require_worker (broker_t *self, char *identity)
{
    assert (identity);

    //  self->workers is keyed off worker identity
    worker_t *worker = zhash_lookup (self->workers, identity);
    if (worker == NULL) {
        worker = (worker_t *) calloc (1, sizeof (worker_t));
        worker->identity = strdup (identity);
        zhash_insert (self->workers, identity, worker);
        if (self->verbose)
            s_console ("I: registering new worker: %s", identity);
    }
    return worker;
}

//  ----------------------------------------------------------------------
//  Deletes worker from all data structures, and destroys worker

static void
s_delete_worker (broker_t *self, worker_t *worker, int disconnect)
{
    assert (worker);
    if (disconnect)
        s_send_to_worker (self, worker, MDPS_DISCONNECT, NULL, NULL);

    zhash_delete (self->workers, worker->identity);
    zlist_remove (self->waiting, worker);
    if (worker->service)
        zlist_remove (worker->service->waiting, worker);
    if (worker->identity)
        free (worker->identity);
    free (worker);
}

//  ----------------------------------------------------------------------

static void
s_waiting_worker (broker_t *self, worker_t *worker)
{
    //  Queue to broker and service waiting lists
    zlist_append (self->waiting, (void *) worker);
    zlist_append (worker->service->waiting, (void *) worker);
    worker->expiry = s_clock () + HEARTBEAT_EXPIRY;
}

//  ----------------------------------------------------------------------

static void
s_process_worker_message (broker_t *self, char *sender, zmsg_t *msg)
{
    assert (zmsg_parts (msg) >= 1);     //  At least, command

    char *command = zmsg_pop (msg);
    int worker_ready = (zhash_lookup (self->workers, sender) != NULL);
    worker_t *worker = s_require_worker (self, sender);

    if (strcmp (command, MDPS_READY) == 0) {
        if (worker_ready)               //  Not first command in session
            s_delete_worker (self, worker, 1);
        else {
            //  Attach worker to service and mark as idle
            worker->service = s_require_service (self, zmsg_pop (msg));
            s_waiting_worker (self, worker);
        }
    }
    else
    if (strcmp (command, MDPS_REPLY) == 0) {
        if (worker_ready) {
            //  Remove & save client return envelope and insert the
            //  protocol header and service name, then rewrap envelope.
            char *client = zmsg_unwrap (msg);
            zmsg_wrap (msg, MDPC_CLIENT, worker->service->name);
            zmsg_wrap (msg, client, "");
            free (client);
            zmsg_send (&msg, self->broker);
            s_waiting_worker (self, worker);
        }
        else
            s_delete_worker (self, worker, 1);
    }
    else
    if (strcmp (command, MDPS_HEARTBEAT) == 0) {
        if (worker_ready)
            worker->expiry = s_clock () + HEARTBEAT_EXPIRY;
        else
            s_delete_worker (self, worker, 1);
    }
    else
    if (strcmp (command, MDPS_DISCONNECT) == 0)
        s_delete_worker (self, worker, 0);
    else {
        s_console ("E: invalid input message (%d)", (int) *command);
        zmsg_dump (msg);
    }
    free (command);
    zmsg_destroy (&msg);
}

//  Return 1 if worker has expired and must be deleted
static int
s_worker_expired (worker_t *worker)
{
    return worker->expiry < s_clock ();
}

//  ----------------------------------------------------------------------
//  Delete any idle workers that haven't pinged us in a while. Workers
//  are oldest to most recent, so we stop at the first alive worker.

static void
s_purge_expired_workers (broker_t *self)
{
    worker_t *worker = zlist_first (self->waiting);
    while (worker) {
        if (!s_worker_expired (worker))
            break;              //  Worker is alive, we're done here
        if (self->verbose)
            s_console ("I: deleting expired worker: %s", worker->identity);

        s_delete_worker (self, worker, 0);
        worker = zlist_first (self->waiting);
    }
}

//  Dispatch requests to waiting workers as possible
static void
s_dispatch_service (broker_t *self, service_t *service, zmsg_t *msg)
{
    assert (service);

    if (msg)                    //  Queue message if any
        zlist_append (service->requests, msg);

    s_purge_expired_workers (self);
    while (zlist_size (service->waiting)
        && zlist_size (service->requests))
    {
        worker_t *worker = zlist_pop (service->waiting);
        zmsg_t *msg = zlist_pop (service->requests);
        s_send_to_worker (self, worker, MDPS_REQUEST, NULL, msg);
        zmsg_destroy (&msg);
    }
}

//  Process a request coming from a client
static void
s_process_client_message (broker_t *self, char *sender, zmsg_t *msg)
{
    assert (zmsg_parts (msg) >= 2);     //  Service name + body
    service_t *service = s_require_service (self, zmsg_pop (msg));
    //  Set reply return address to client sender
    zmsg_wrap (msg, sender, "");
    //  Takes ownership of the message
    s_dispatch_service (self, service, msg);
}


//  ----------------------------------------------------------------------

int main (void)
{
    s_version_assert (2, 1);

    //  Initialize broker state
    broker_t *self = (broker_t *) calloc (1, sizeof (broker_t));
    self->context = zmq_init (1);
    self->broker = zmq_socket (self->context, ZMQ_XREP);
    self->verbose = VERBOSE_BROKER;
    self->services = zhash_new ();
    self->workers = zhash_new ();
    self->waiting = zlist_new ();
    self->heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;

    //  We use a single socket for both clients and workers
    char *endpoint = "tcp://*:5555";
    zmq_bind (self->broker, endpoint);

    s_console ("I: MDP broker ready at %s", endpoint);

    //  Get and process messages forever
    while (1) {
        zmq_pollitem_t items [] = { { self->broker,  0, ZMQ_POLLIN, 0 } };
        zmq_poll (items, 1, HEARTBEAT_INTERVAL * 1000);

        //  Process next input message, if any
        if (items [0].revents & ZMQ_POLLIN) {
            zmsg_t *msg = zmsg_recv (self->broker);
            if (self->verbose) {
                s_console ("I: received message:");
                zmsg_dump (msg);
            }
            char *sender = zmsg_unwrap (msg);
            char *header = zmsg_pop (msg);

            if (strcmp (header, MDPC_CLIENT) == 0)
                s_process_client_message (self, sender, msg);
            else
            if (strcmp (header, MDPS_WORKER) == 0)
                s_process_worker_message (self, sender, msg);
            else {
                s_console ("E: invalid message:");
                zmsg_dump (msg);
                zmsg_destroy (&msg);
            }
            free (sender);
            free (header);
        }
        //  Disconnect and delete any expired workers
        //  Send heartbeats to idle workers if needed
        if (s_clock () > self->heartbeat_at) {
            s_purge_expired_workers (self);
            worker_t *worker = zlist_first (self->waiting);
            while (worker) {
                s_send_to_worker (self, worker, MDPS_HEARTBEAT, NULL, NULL);
                worker = zlist_next (self->waiting);
            }
            self->heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;
        }
    }
    //  We never exit the main loop
    return 0;
}
