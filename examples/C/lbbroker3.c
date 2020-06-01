//  Load-balancing broker
//  Demonstrates use of the CZMQ API and reactor style
//
//  The client and worker tasks are similar to the previous example.
//  .skip

#include "czmq.h"
#define NBR_CLIENTS 10
#define NBR_WORKERS 3
#define WORKER_READY   "\001"      //  Signals worker is ready

//  Basic request-reply client using REQ socket
//
static void
client_task (zsock_t *pipe, void *args)
{
    // Signal ready
    zsock_signal(pipe, 0);

    zsock_t *client = zsock_new_req ("ipc://frontend.ipc");
    zpoller_t *poller  = zpoller_new (pipe, client, NULL);
    zpoller_set_nonstop(poller,true);

    //  Send request, get reply
    while (true) {
        zstr_send (client, "HELLO");

        zsock_t *ready = zpoller_wait (poller, -1);
        if (ready == NULL) continue;   // Interrupted
        else if (ready == pipe) break; // Shutdown
        else assert(ready == client);  // Data Available

        char *reply = zstr_recv (client);
        if (!reply)
            break;
        printf ("Client: %s\n", reply);
        free (reply);
        sleep (1);
    }

    zpoller_destroy(&poller);
    zsock_destroy(&client);
}

//  Worker using REQ socket to do load-balancing
//
static void
worker_task (zsock_t *pipe, void *args)
{
    // Signal ready
    zsock_signal(pipe, 0);

    zsock_t *worker = zsock_new_req ("ipc://backend.ipc");
    zpoller_t *poller = zpoller_new (pipe, worker, NULL);
    zpoller_set_nonstop(poller, true);

    //  Tell broker we're ready for work
    zframe_t *frame = zframe_new (WORKER_READY, 1);
    zframe_send (&frame, worker, 0);

    //  Process messages as they arrive
    while (true) {
        zsock_t *ready = zpoller_wait (poller, -1);
        if (ready == NULL) continue;   // Interrupted
        else if (ready == pipe) break; // Shutdown
        else assert(ready == worker);  // Data Available

        zmsg_t *msg = zmsg_recv (worker);
        if (!msg)
            break;              //  Interrupted
        zframe_print (zmsg_last (msg), "Worker: ");
        zframe_reset (zmsg_last (msg), "OK", 2);
        zmsg_send (&msg, worker);
    }

    zpoller_destroy(&poller);
    zsock_destroy(&worker);
}

//  .until
//  Our load-balancer structure, passed to reactor handlers
typedef struct {
    zsock_t *frontend;          //  Listen to clients
    zsock_t *backend;           //  Listen to workers
    zlist_t *workers;           //  List of ready workers
} lbbroker_t;

//  .split reactor design
//  In the reactor design, each time a message arrives on a socket, the
//  reactor passes it to a handler function. We have two handlers; one
//  for the frontend, one for the backend:

//  Handle input from client, on frontend
static int s_handle_frontend (zloop_t *loop, zsock_t *reader, void *arg)
{
    lbbroker_t *self = (lbbroker_t *) arg;
    zmsg_t *msg = zmsg_recv (self->frontend);
    if (msg) {
        zmsg_pushmem (msg, NULL, 0); // delimiter
        zmsg_push (msg, (zframe_t *) zlist_pop (self->workers));
        zmsg_send (&msg, self->backend);

        //  Cancel reader on frontend if we went from 1 to 0 workers
        if (zlist_size (self->workers) == 0) {
            zloop_reader_end (loop, self->frontend);
        }
    }
    return 0;
}

//  Handle input from worker, on backend
static int s_handle_backend (zloop_t *loop, zsock_t *reader, void *arg)
{
    //  Use worker identity for load-balancing
    lbbroker_t *self = (lbbroker_t *) arg;
    zmsg_t *msg = zmsg_recv (self->backend);
    if (msg) {
        zframe_t *identity = zmsg_pop (msg);
        zframe_t *delimiter = zmsg_pop (msg);
        zframe_destroy (&delimiter);
        zlist_append (self->workers, identity);

        //  Enable reader on frontend if we went from 0 to 1 workers
        if (zlist_size (self->workers) == 1) {
            zloop_reader (loop, self->frontend, s_handle_frontend, self);
        }
        //  Forward message to client if it's not a READY
        zframe_t *frame = zmsg_first (msg);
        if (memcmp (zframe_data (frame), WORKER_READY, 1) == 0)
            zmsg_destroy (&msg);
        else
            zmsg_send (&msg, self->frontend);
    }
    return 0;
}

//  .split main task
//  And the main task now sets up child tasks, then starts its reactor.
//  If you press Ctrl-C, the reactor exits and the main task shuts down.
//  Because the reactor is a CZMQ class, this example may not translate
//  into all languages equally well.

int main (void)
{
    lbbroker_t *self = (lbbroker_t *) zmalloc (sizeof (lbbroker_t));
    self->frontend = zsock_new_router ("ipc://frontend.ipc");
    self->backend = zsock_new_router ("ipc://backend.ipc");

    zactor_t *actors[NBR_CLIENTS + NBR_WORKERS];
    int actor_nbr = 0;

    int client_nbr;
    for (client_nbr = 0; client_nbr < NBR_CLIENTS; client_nbr++)
        actors[actor_nbr++] = zactor_new (client_task, NULL);
    int worker_nbr;
    for (worker_nbr = 0; worker_nbr < NBR_WORKERS; worker_nbr++)
        actors[actor_nbr++] = zactor_new (worker_task, NULL);

    //  Queue of available workers
    self->workers = zlist_new ();

    //  Prepare reactor and fire it up
    zloop_t *reactor = zloop_new ();
    zloop_reader (reactor, self->backend, s_handle_backend, self);
    zloop_start  (reactor);
    zloop_destroy (&reactor);
    for (actor_nbr = 0; actor_nbr < NBR_CLIENTS + NBR_WORKERS; actor_nbr++)
        zactor_destroy(&actors[actor_nbr]);

    //  When we're done, clean up properly
    while (zlist_size (self->workers)) {
        zframe_t *frame = (zframe_t *) zlist_pop (self->workers);
        zframe_destroy (&frame);
    }
    zlist_destroy (&self->workers);
    zsock_destroy (&self->frontend);
    zsock_destroy (&self->backend);
    free (self);
    return 0;
}
