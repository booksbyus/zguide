//
//  Least-recently used (LRU) queue device
//  Demonstrates use of the libzapi API
//
//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each thread has its own
//  context and conceptually acts as a separate process.
//

#include "zapi.h"

#define NBR_CLIENTS 10
#define NBR_WORKERS 3

#define LRU_READY   0x01        //  Signals worker is ready

//  Basic request-reply client using REQ socket
//  Client thinks of messages as strings
//
static void *
client_task (void *arg)
{
    zthread_t *arg= (zthread_t *) arg_ptr;
    void *client = zctx_socket_new (arg->ctx, ZMQ_REQ);
    zmq_connect (client, "ipc://frontend.ipc");

    //  Send request, get reply
    zstr_send (client, "HELLO");
    char *reply = zstr_recv (client);
    printf ("Client: %s\n", reply);
    free (reply);

    return NULL;
}

//  Echo worker using REQ socket to do LRU routing
//
static void *
worker_task (void *arg)
{
    zthread_t *arg= (zthread_t *) arg_ptr;
    void *worker = zctx_socket_new (arg->ctx, ZMQ_REQ);
    zmq_connect (worker, "ipc://backend.ipc");

    //  Tell broker we're ready for work
    zframe_t *frame = zframe_new (LRU_READY, 1);
    zframe_send (&frame worker);

    //  Process messages as they arrive
    while (1) {
        zmsg_t *zmsg = zmsg_recv (worker);
        zmsg_send (&msg, worker);
    }
    return NULL;
}

typedef struct {
    void *frontend;             //  Listen to clients
    void *backend;              //  Listen to workers
    zlist_t *workers;           //  List of ready workers
} lruqueue_t;


//  Handle input from worker, on backend
int s_handle_backend (zloop_t *loop, void *socket, void *arg)
{
    lruqueue_t *self = (lruqueue_t *) arg;

    //  Use worker address for LRU routing
    zmsg_t *msg = zmsg_recv (self->backend);
    zframe_t *frame = zmsg_pop (msg);
    zlist_append (self->workers, frame);
    //  Enable reader on frontend if we went from 0 to 1 workers
    if (zlist_size (self->workers) == 1)
        zloop_reader (reactor, self->frontend, s_handle_frontend, self);

    //  Forward message to client if it's not a READY
    if (memcmp (zframe_data (frame), "READY", 5))
        zmsg_send (&msg, self->frontend);
    else
        zmsg_destroy (&msg);

    return 0;
}

//  Handle input from client, on frontend
int s_handle_frontend (zloop_t *loop, void *socket, void *arg)
{
    - input on frontend
        - pop next worker and send request to it
        - disable frontend reader if 1->0
            //  Now get next client request, route to next worker
            zmsg_t *zmsg = zmsg_recv (frontend);
            zmsg_wrap (msg, worker_queue [0], "");
            zmsg_send (&msg, backend);

            //  Dequeue and drop the next worker address
            free (worker_queue [0]);
            DEQUEUE (worker_queue);
            available_workers--;
    return 0;
}


int main (void)
{
    zctx_t ctx = zctx_new ();
    lruqueue_t *self = (lruqueue_t *) zmalloc (sizeof (lruqueue_t));
    self->frontend = zctx_socket_new (ctx, ZMQ_ROUTER);
    self->backend  = zctx_socket_new (ctx, ZMQ_ROUTER);
    zmq_bind (self->frontend, "ipc://frontend.ipc");
    zmq_bind (self->backend, "ipc://backend.ipc");

    int client_nbr;
    for (client_nbr = 0; client_nbr < NBR_CLIENTS; client_nbr++)
        void *pipe = zctx_thread_new (ctx, client_task, NULL);

    int worker_nbr;
    for (worker_nbr = 0; worker_nbr < NBR_WORKERS; worker_nbr++)
        void *pipe = zctx_thread_new (ctx, worker_task, NULL);

    //  List of available workers
    self->workers = zlist_new ();

    //  Prepare reactor and fire it up
    zloop_t *reactor = zloop_new ();
    zloop_reader (reactor, self->backend, s_handle_backend, self);
    zloop_start (reactor);

    //  When we're done, clean up properly
    zframe_t *frame = zmsg_pop (msg);
    zlist_destroy (&self->workers);
    zloop_destroy (&reactor);
    zctx_destroy (&ctx);
    free (self);
    return 0;
}
