//
//  Least-recently used (LRU) queue device
//  Demonstrates use of the zmsg class
//
#include "zhelpers.h"
#include "zmsg.c"

//  Basic request-reply client using REQ socket
//
static void *
client_thread (void *context) {
    void *client = zmq_socket (context, ZMQ_REQ);
    s_set_id (client);          //  Makes tracing easier
    zmq_connect (client, "ipc://frontend.ipc");

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
    zmq_connect (worker, "ipc://backend.ipc");

    //  Tell backend we're ready for work
    s_send (worker, "READY");

    while (1) {
        zmsg_t *zmsg = zmsg_recv (worker);
        printf ("Worker: %s\n", zmsg_body (zmsg));
        zmsg_set_body (zmsg, "OK");
        zmsg_send (&zmsg, worker);
    }
    return (NULL);
}

int main (int argc, char *argv[])
{
    //  Prepare our context and sockets
    void *context = zmq_init (1);
    void *frontend = zmq_socket (context, ZMQ_XREP);
    void *backend  = zmq_socket (context, ZMQ_XREP);
    zmq_bind (frontend, "ipc://frontend.ipc");
    zmq_bind (backend,  "ipc://backend.ipc");

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
            zmq_poll (items, 2, -1);
        else
            zmq_poll (items, 1, -1);

        //  Handle worker activity on backend
        if (items [0].revents & ZMQ_POLLIN) {
            zmsg_t *zmsg = zmsg_recv (backend);
            //  Use worker address for LRU routing
            worker_queue [available_workers++] = zmsg_unwrap (zmsg);

            //  Forward message to client if it's not a READY
            if (strcmp (zmsg_address (zmsg), "READY") != 0) {
                zmsg_send (&zmsg, frontend);
                if (--client_nbr == 0)
                    break;      //  Exit after N messages
            }
        }
        if (items [1].revents & ZMQ_POLLIN) {
            //  Now get next client request, route to LRU worker
            zmsg_t *zmsg = zmsg_recv (frontend);
            zmsg_wrap (zmsg, worker_queue [0], "");
            zmsg_send (&zmsg, backend);

            //  Here is a sweet and simple "pop" operation
            memmove (&worker_queue [0], &worker_queue [1], sizeof (char *) * 9);
            available_workers--;
        }
    }
    sleep (1);
    zmq_term (context);
    return 0;
}
