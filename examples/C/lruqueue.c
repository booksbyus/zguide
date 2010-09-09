//
//  Least-recently used (LRU) queue device
//  Clients and workers are shown here in-process
//
#include "zhelpers.h"

//  Basic request-reply client using REQ socket
//
static void *
client_thread (void *context) {
    void *client = zmq_socket (context, ZMQ_REQ);
    s_set_id (client);          //  Makes tracing easier
    zmq_connect (client, "ipc://frontend");

    //  Send request, get reply
    s_send (client, "HELLO");
    char *reply = s_recv (client);
    printf ("Client: %s\n", reply);
    free (reply);
    return (NULL);
}

//  Worker using REQ socket to do LRU routing
//
static void *
worker_thread (void *context) {
    void *worker = zmq_socket (context, ZMQ_REQ);
    s_set_id (worker);          //  Makes tracing easier
    zmq_connect (worker, "ipc://backend");

    //  Tell backend we're ready for work
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
    return (NULL);
}

int main (int argc, char *argv[])
{
    //  Prepare our context and sockets
    void *context = zmq_init (1);
    void *frontend = zmq_socket (context, ZMQ_XREP);
    void *backend  = zmq_socket (context, ZMQ_XREP);
    zmq_bind (frontend, "ipc://frontend");
    zmq_bind (backend,  "ipc://backend");

    int client_nbr;
    for (client_nbr = 0; client_nbr < 10; client_nbr++) {
        pthread_t client;
        pthread_create (&client, NULL, client_thread, context);
    }
    int worker_nbr;
    for (worker_nbr = 0; worker_nbr < 3; worker_nbr++) {
        pthread_t worker;
        pthread_create (&worker, NULL, worker_thread, context);
    }
    //  Logic of LRU loop
    //  - Poll backend always, frontend only if 1+ worker ready
    //  - If worker replies, queue worker as ready and forward reply
    //    to client if necessary
    //  - If client requests, pop next worker and send request to it
    //
    //  A very simple queue structure with known max size
    int available_workers = 0;
    char *worker_queue [10];

    while (1) {
        //  Initialize poll set
        zmq_pollitem_t items [2] = {
            //  Always poll for worker activity on backend
            { backend,  0, ZMQ_POLLIN, 0 },
            //  Poll front-end only if we have available workers
            { frontend, 0, ZMQ_POLLIN, 0 }
        };
        if (available_workers)
            zmq_poll (&items [0], 2, -1);
        else
            zmq_poll (&items [0], 1, -1);

        //  Handle worker activity on backend
        if (items [0].revents & ZMQ_POLLIN) {
            //  Queue worker address for LRU routing
            char *worker_addr = s_recv (backend);
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
                    break;
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

            //  Here is a sweet and simple "pop" operation
            char *worker_addr = worker_queue [0];
            memmove (&worker_queue [0], &worker_queue [1], sizeof (char *) * 9);
            available_workers--;

            s_sendmore (backend, worker_addr);
            s_sendmore (backend, "");
            s_sendmore (backend, client_addr);
            s_sendmore (backend, "");
            s_send     (backend, request);

            free (worker_addr);
            free (client_addr);
            free (request);
        }
    }
    sleep (1);
    return 0;
}
