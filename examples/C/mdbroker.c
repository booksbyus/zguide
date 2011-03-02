//
//  Majordomo Protocol broker
//  A minimal implementation
//
#include "zmsg.class"
#include "zlist.class"
#include "zhash.class"
#include "mdp.h"

//  We'd normally pull these from config data

#define HEARTBEAT_LIVENESS  3       //  3-5 is reasonable
#define HEARTBEAT_INTERVAL  5000    //  msecs
#define VERBOSE_BROKER      1       //  Trace activity?

//  This defines a single broker
typedef struct {
    void *context;              //  0MQ context
    void *broker;               //  Socket for clients & workers
    int verbose;                //  Print activity to stdout
    zhash_t *services;          //  Hash of known services
    zhash_t *workers;           //  Hash of known workers
    zlist_t *idle_workers;      //  List of idle workers
    uint64_t heartbeat_at;      //  When to send HEARTBEAT
} broker_t;

//  This defines a single service
typedef struct {
    char *name;                 //  Service name
    zlist_t *requests;          //  List of client requests
    zlist_t *workers;           //  All workers for this service
    zlist_t *idle_workers;      //  List of idle workers
} service_t;

//  This defines one worker, idle or active
typedef struct {
    char *identity;             //  Address of worker
    char *service;              //  Service name
    int64_t expiry;             //  Expires at unless heartbeat
} worker_t;


//  Locate or create new service entry
static service_t *
s_require_service (broker_t *self, char *name)
{
    assert (name);
    service_t *service = zhash_lookup (self->services, name);
    if (service == NULL) {
        service = (service_t *) malloc (sizeof (service_t));
        service->name = strdup (name);
        service->requests = zlist_new ();
        service->workers = zlist_new ();
        service->idle_workers = zlist_new ();
        zhash_insert (self->services, name, service);
        if (self->verbose) {
            s_console ("I: received message:");
            s_console ("I: registering new service: '%s'", name);
        }
    }
}

//  Dispatch service requests to workers as possible
static void
s_dispatch_service (broker_t *self, char *service)
{
#if 0
    char *worker_id = zlist_first (service->idle_workers);
    while (worker_id) {

        worker_t *worker = zhash_lookup (self->workers, name);
        assert (worker);
        if (worker->expiry > s_clock ())
            if message, route
            else break
        else
            disconnect & kill

        worker_id = zlist_next (service->idle_workers);
    }
    &&     zlist_size ()) {

    if (zlist_size (service->requests))

        msg_t *msg = zlist_pop (service->requests);
        char *identity = zlist_pop (service->idle_workers);

       /* Request:
        * Address: workerid
        * Frame 0: "MDPW01"
        * Frame 1: 0x02 (one byte, representing REQUEST)
        * Frame 2: Client address (envelope stack)
        * Frame 3: Empty (zero bytes, envelope delimiter)
        * Frames 4+: Request body (opaque binary)
        */

        s_console ("B: dispatch to %s", identity);
        zmsg_destroy (&msg);
        free (identity);
    }
#endif
}

//  Process a request coming from a client
static void
s_process_client_message (broker_t *self, char *sender, zmsg_t *msg)
{
    s_console ("s_process_client_message");
#if 0
    assert (zmsg_parts (msg) >= 2);     //  Service name + body

    char *service_name = zmsg_pop (msg);
    service_t *service = s_require_service (self, service_name);
    free (service_name);

    //  Append request to service request queue
    zlist_append (service->requests, msg);
    s_dispatch_service (self, service);
#endif
}

//  ----------------------------------------------------------------------

static worker_t *
s_require_worker (broker_t *self, char *identity, char *service)
{
    assert (identity);
    worker_t *worker = zhash_lookup (self->workers, identity);
    if (worker == NULL) {
        worker = (worker_t *) malloc (sizeof (worker_t));
        worker->identity = strdup (identity);
        worker->service = strdup (service);
        zhash_insert (self->workers, identity, worker);
        if (self->verbose)
            s_console ("I: registering new worker: '%s'", identity);
    }
    return worker;
}


//  --------------------------------------------------------------------------
//  Send message to worker
//  If no _msg is provided, creates one internally

static void
s_send_to_worker (
    broker_t *self, worker_t *worker,
    char *command, char *option, zmsg_t *_msg)
{
    zmsg_t *msg = _msg? zmsg_dup (_msg): zmsg_new ();

    //  Stack protocol envelope to start of message
    if (option)
        zmsg_push (msg, option);
    zmsg_push (msg, command);
    zmsg_push (msg, MDPS_WORKER);

    //  Stack routing envelope to start of message
    zmsg_push (msg, worker->identity);

    if (self->verbose) {
        s_console ("I: sending %s to worker %s",
            mdps_commands [(int) *command], worker->identity);
        zmsg_dump (msg);
    }
    zmsg_send (&msg, self->broker);
}


//  ----------------------------------------------------------------------

static void
s_process_worker_message (broker_t *self, char *sender, zmsg_t *msg)
{
    s_console ("I: process worker message");
    zmsg_dump (msg);
    assert (zmsg_parts (msg) >= 1);     //  At least, command

    char *command = zmsg_pop (msg);
    s_console (mdps_commands [(int) *command]);

    if (strcmp (command, MDPS_READY) == 0) {
        char *service = zmsg_pop (msg);
        worker_t *worker = s_require_worker (self, sender, service);
        free (service);
        zlist_append (self->idle_workers, (void *) worker);
        worker->expiry = s_clock () + HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS;
    }
    else
    if (strcmp (command, MDPS_REPLY) == 0) {
    }
    else
    if (strcmp (command, MDPS_HEARTBEAT) == 0) {
    }
    else
    if (strcmp (command, MDPS_DISCONNECT) == 0) {
    }
    else {
        s_console ("E: invalid input message (%d)", (int) *command);
        zmsg_dump (msg);
    }
    free (command);
#if 0

    ready
        - s_require_worker
        - append to idle queue
        worker->expiry = s_clock () + HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS;
    heartbeat
        - s_require_worker
        worker->expiry = s_clock () + HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS;

    reply
        - assert worker already exists
       /* Reply:
        * Address: clientid
        * Frame 0: Empty
        * Frame 1: "MDPC01"
        * Frame 2: Service name
        * Frames 3+: Reply body
        */
        //  send message back to originating client
        zmsg_send (&msg, broker);
        - append to idle queue
    }
#endif
}

//  ----------------------------------------------------------------------
//  Delete any idle workers that haven't pinged us in a while. Workers
//  are oldest to most recent, so we stop at the first alive worker.

static void
s_purge_expired_workers (broker_t *self)
{
    worker_t *worker = zlist_first (self->idle_workers);
    while (worker) {
        if (worker->expiry > s_clock ())
            break;              //  Worker is alive, we're done here
        if (self->verbose)
            s_console ("I: deleting expired worker: %s", worker->identity);

        worker_t *popped = zlist_pop (self->idle_workers);
        assert (worker == popped);

        //  Delete from idle_list
        //  Delete from service
        worker = zlist_first (self->idle_workers);
    }
}


//  ----------------------------------------------------------------------

int main (void)
{
    s_version_assert (2, 1);

    //  Initialize broker state
    broker_t *self = (broker_t *) malloc (sizeof (broker_t));
    self->context = zmq_init (1);
    self->broker = zmq_socket (self->context, ZMQ_XREP);
    self->verbose = VERBOSE_BROKER;
    self->services = zhash_new ();
    self->workers = zhash_new ();
    self->idle_workers = zlist_new ();
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
            }
            free (sender);
            free (header);
            zmsg_destroy (&msg);
        }
        //  Disconnect and delete any expired workers
        //  Send heartbeats to idle workers if needed
        if (s_clock () > self->heartbeat_at) {
            s_purge_expired_workers (self);
            worker_t *worker = zlist_first (self->idle_workers);
            while (worker) {
                s_send_to_worker (self, worker, MDPS_HEARTBEAT, NULL, NULL);
                worker = zlist_next (self->idle_workers);
            }
            self->heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;
        }
    }
    //  We never exit the main loop
    return 0;
}
