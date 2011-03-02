//
//  Majordomo broker
//  A minimal implementation
//
#include "zmsg.class"
#include "zlist.class"
#include "zhash.class"
#include "mdp.h"

#define HEARTBEAT_LIVENESS  3       //  3-5 is reasonable
#define HEARTBEAT_INTERVAL  1000    //  msecs


//  This defines a single broker
typedef struct {
    void *context;              //  0MQ context
    void *broker;               //  Socket for clients & workers
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

        printf ("B: dispatch to %s\n", identity);
        zmsg_destroy (&msg);
        free (identity);
    }
#endif
}

//  Process a request coming from a client
static void
s_process_client_message (broker_t *self, char *sender, zmsg_t *msg)
{
    printf ("s_process_client_message\n");
    zmsg_dump (msg);
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

static void
s_process_worker_message (broker_t *self, char *sender, zmsg_t *msg)
{
    printf ("s_process_worker_message\n");
    zmsg_dump (msg);
#if 0
    assert (zmsg_parts (msg) >= 2);     //  Service name + body

    //  Lookup worker, create if it's a new one
    worker_t *worker = zhash_lookup (self->workers, name);
    if (worker == NULL) {
        worker = (worker_t *) malloc (sizeof (worker_t));
        worker->identity = strdup (sender);
        zhash_insert (self->workers, sender, worker);
    }
    ready
        - append to idle queue
        worker->expiry = s_clock () + HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS;
    heartbeat
        worker->expiry = s_clock () + HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS;

    reply
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

int main (void)
{
    s_version_assert (2, 1);

    //  Initialize broker state
    broker_t *self = (broker_t *) malloc (sizeof (broker_t));
    self->context = zmq_init (1);
    self->broker = zmq_socket (self->context, ZMQ_XREP);
    self->services = zhash_new ();
    self->workers = zhash_new ();
    self->idle_workers = zlist_new ();
    self->heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;

    //  We use a single socket for both clients and workers
    char *endpoint = "tcp://*:5555";
    zmq_bind (self->broker, endpoint);
    printf ("I: Majordomo broker ready at %s\n", endpoint);

    //  Get and process messages forever
    while (1) {
        zmq_pollitem_t items [] = { { self->broker,  0, ZMQ_POLLIN, 0 } };
        zmq_poll (items, 1, HEARTBEAT_INTERVAL * 1000);

        //  Process next input message, if any
        if (items [0].revents & ZMQ_POLLIN) {
            zmsg_t *msg = zmsg_recv (self->broker);
            char *sender = zmsg_unwrap (msg);
            char *header = zmsg_pop (msg);

            if (strcmp (header, MDPC_CLIENT) == 0)
                s_process_client_message (self, sender, msg);
            else
            if (strcmp (header, MDPS_WORKER) == 0)
                s_process_worker_message (self, sender, msg);
            else {
                printf ("E: invalid message\n");
                zmsg_dump (msg);
            }
            free (sender);
            free (header);
            zmsg_destroy (&msg);
        }
        //  Send heartbeats to idle workers if needed
        if (s_clock () > self->heartbeat_at) {
            char *worker = zlist_first (self->idle_workers);
            while (worker) {
                zmsg_t *msg = zmsg_new ();
                zmsg_append (msg, worker);
                zmsg_append (msg, MDPS_WORKER);
                zmsg_append (msg, MDPS_HEARTBEAT);
                zmsg_send (&msg, self->broker);
                worker = zlist_next (self->idle_workers);
            }
            self->heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;
        }
    }
    //  We never exit the main loop
    return 0;
}
