//
//  Least-recently used (LRU) queue device
//  Clients and workers are shown here in-process
//
//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each thread has its own
//  context and conceptually acts as a separate process.
//
//  Changes for 2.1:
//  - added version assertion
//  - use separate contexts for each thread
//  - close sockets in each child thread
//  - call zmq_term in each thread before ending
//  - removed sleep(1) at end of main thread
//
#include "zhelpers.h"

#define NBR_CLIENTS 10
#define NBR_WORKERS 3

//  A simple dequeue operation for queue implemented as array
#define DEQUEUE(q) memmove (&(q)[0], &(q)[1], sizeof (q) - sizeof (q [0]))

//  Basic request-reply client using REQ socket
//
static void *
client_thread (void *args) {
    void *context = zmq_init (1);
    void *client = zmq_socket (context, ZMQ_REQ);
    s_set_id (client);          //  Makes tracing easier
    zmq_connect (client, "ipc://frontend.ipc");

    //  Send request, get reply
    s_send (client, "HELLO");
    char *reply = s_recv (client);
    printf ("Client: %s\n", reply);
    free (reply);
    zmq_close (client);
    zmq_term (context);
    return NULL;
}

//  Worker using REQ socket to do LRU routing
//
static void *
worker_thread (void *args) {
    void *context = zmq_init (1);
    void *worker = zmq_socket (context, ZMQ_REQ);
    s_set_id (worker);          //  Makes tracing easier
    zmq_connect (worker, "ipc://backend.ipc");

    //  Tell broker we're ready for work
    s_send (worker, "READY");

    while (1) {
        //  Read and save all frames until we get an empty frame
        //  In this example there is only 1 but it could be more
        char *address = s_recv (worker);
        char *empty = s_recv (worker);
        assert (*empty == 0);
        free (empty);

        //  Get request, send reply
        char *request = s_recv (worker);
        printf ("Worker: %s\n", request);
        free (request);

        s_sendmore (worker, address);
        s_sendmore (worker, "");
        s_send     (worker, "OK");
        free (address);
    }
    zmq_close (worker);
    zmq_term (context);
    return NULL;
}

int main (void) 
{
    s_version_assert (2, 1);

    //  Prepare our context and sockets
    void *context = zmq_init (1);
    void *frontend = zmq_socket (context, ZMQ_XREP);
    void *backend  = zmq_socket (context, ZMQ_XREP);
    zmq_bind (frontend, "ipc://frontend.ipc");
    zmq_bind (backend,  "ipc://backend.ipc");

    int client_nbr;
    for (client_nbr = 0; client_nbr < NBR_CLIENTS; client_nbr++) {
        pthread_t client;
        pthread_create (&client, NULL, client_thread, NULL);
    }
    int worker_nbr;
    for (worker_nbr = 0; worker_nbr < NBR_WORKERS; worker_nbr++) {
        pthread_t worker;
        pthread_create (&worker, NULL, worker_thread, NULL);
    }
    //  Logic of LRU loop
    //  - Poll backend always, frontend only if 1+ worker ready
    //  - If worker replies, queue worker as ready and forward reply
    //    to client if necessary
    //  - If client requests, pop next worker and send request to it

    //  Queue of available workers
    int available_workers = 0;
    char *worker_queue [10];

    while (1) {
        //  Initialize poll set
        zmq_pollitem_t items [] = {
            //  Always poll for worker activity on backend
            { backend,  0, ZMQ_POLLIN, 0 },
            //  Poll front-end only if we have available workers
            { frontend, 0, ZMQ_POLLIN, 0 }
        };
        if (available_workers)
            zmq_poll (items, 2, -1);
        else
            zmq_poll (items, 1, -1);

        //  Handle worker activity on backend
        if (items [0].revents & ZMQ_POLLIN) {
            //  Queue worker address for LRU routing
            char *worker_addr = s_recv (backend);
            assert (available_workers < NBR_WORKERS);
            worker_queue [available_workers++] = worker_addr;

            //  Second frame is empty
            char *empty = s_recv (backend);
            assert (empty [0] == 0);
            free (empty);

            //  Third frame is READY or else a client reply address
            char *client_addr = s_recv (backend);

            //  If client reply, send rest back to frontend
            if (strcmp (client_addr, "READY") != 0) {
                empty = s_recv (backend);
                assert (empty [0] == 0);
                free (empty);
                char *reply = s_recv (backend);
                s_sendmore (frontend, client_addr);
                s_sendmore (frontend, "");
                s_send     (frontend, reply);
                free (reply);
                if (--client_nbr == 0)
                    break;      //  Exit after N messages
            }
            free (client_addr);
        }
        if (items [1].revents & ZMQ_POLLIN) {
            //  Now get next client request, route to LRU worker
            //  Client request is [address][empty][request]
            char *client_addr = s_recv (frontend);
            char *empty = s_recv (frontend);
            assert (empty [0] == 0);
            free (empty);
            char *request = s_recv (frontend);

            s_sendmore (backend, worker_queue [0]);
            s_sendmore (backend, "");
            s_sendmore (backend, client_addr);
            s_sendmore (backend, "");
            s_send     (backend, request);

            free (client_addr);
            free (request);

            //  Dequeue and drop the next worker address
            free (worker_queue [0]);
            DEQUEUE (worker_queue);
            available_workers--;
        }
    }
    zmq_close (frontend);
    zmq_close (backend);
    zmq_term (context);
    return 0;
}
