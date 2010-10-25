//
//  Least-recently used (LRU) queue device
//  Demonstrates use of the zmsg class
//
#include "zhelpers.h"
#include "zmsg.c"

#define NBR_CLIENTS 10
#define NBR_WORKERS 3

//  A simple dequeue operation for queue implemented as array
#define DEQUEUE(q) memmove (&(q)[0], &(q)[1], sizeof (q) - sizeof (q [0]))

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
    return NULL;
}

//  Worker using REQ socket to do LRU routing
//
static void *
worker_thread (void *context) {
    void *worker = zmq_socket (context, ZMQ_REQ);
    s_set_id (worker);          //  Makes tracing easier
    zmq_connect (worker, "ipc://backend.ipc");

    //  Tell broker we're ready for work
    s_send (worker, "READY");

    while (1) {
        zmsg_t *zmsg = zmsg_recv (worker);
        printf ("Worker: %s\n", zmsg_body (zmsg));
        zmsg_body_set (zmsg, "OK");
        zmsg_send (&zmsg, worker);
    }
    return NULL;
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
    for (client_nbr = 0; client_nbr < NBR_CLIENTS; client_nbr++) {
        pthread_t client;
        pthread_create (&client, NULL, client_thread, context);
    }
    int worker_nbr;
    for (worker_nbr = 0; worker_nbr < NBR_WORKERS; worker_nbr++) {
        pthread_t worker;
        pthread_create (&worker, NULL, worker_thread, context);
    }
    //  Logic of LRU loop
    //  - Poll backend always, frontend only if 1+ worker ready
    //  - If worker replies, queue worker as ready and forward reply
    //    to client if necessary
    //  - If client requests, pop next worker and send request to it

    //  Queue of available workers
    int available_workers = 0;
    char *worker_queue [NBR_WORKERS];

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
            zmsg_t *zmsg = zmsg_recv (backend);
            //  Use worker address for LRU routing
            assert (available_workers < NBR_WORKERS);
            worker_queue [available_workers++] = zmsg_unwrap (zmsg);

            //  Forward message to client if it's not a READY
            if (strcmp (zmsg_address (zmsg), "READY") == 0)
                zmsg_destroy (&zmsg);
            else {
                zmsg_send (&zmsg, frontend);
                if (--client_nbr == 0)
                    break;      //  Exit after N messages
            }
        }
        if (items [1].revents & ZMQ_POLLIN) {
            //  Now get next client request, route to next worker
            zmsg_t *zmsg = zmsg_recv (frontend);
            zmsg_wrap (zmsg, worker_queue [0], "");
            zmsg_send (&zmsg, backend);

            //  Dequeue and drop the next worker address
            free (worker_queue [0]);
            DEQUEUE (worker_queue);
            available_workers--;
        }
    }
    sleep (1);
    zmq_close (frontend);
    zmq_close (backend);
    zmq_term (context);
    return 0;
}
