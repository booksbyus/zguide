//
//  Pirate work queue, design 1
//
//  This has no recovery mechanisms at all, it depends on the client for
//  recovery. Runs forever.
//
#include "zhelpers.h"
#include "zmsg.c"

#define MAX_WORKERS 100

//  A simple dequeue operation for queue implemented as array
#define DEQUEUE(q) memmove (&(q)[0], &(q)[1], sizeof (q) - sizeof (q [0]))

int main (void)
{
    s_version_assert (2, 1);

    //  Prepare our context and sockets
    void *context = zmq_init (1);
    void *frontend = zmq_socket (context, ZMQ_XREP);
    void *backend  = zmq_socket (context, ZMQ_XREP);
    zmq_bind (frontend, "tcp://*:5555");    //  For clients
    zmq_bind (backend,  "tcp://*:5556");    //  For workers

    //  Queue of available workers
    int available_workers = 0;
    char *worker_queue [MAX_WORKERS];

    while (1) {
        zmq_pollitem_t items [] = {
            { backend,  0, ZMQ_POLLIN, 0 },
            { frontend, 0, ZMQ_POLLIN, 0 }
        };
        //  Poll frontend only if we have available workers
        if (available_workers)
            zmq_poll (items, 2, -1);
        else
            zmq_poll (items, 1, -1);

        //  Handle worker activity on backend
        if (items [0].revents & ZMQ_POLLIN) {
            zmsg_t *zmsg = zmsg_recv (backend);
            //  Use worker address for LRU routing
            assert (available_workers < MAX_WORKERS);
            worker_queue [available_workers++] = zmsg_unwrap (zmsg);

            //  Return reply to client if it's not a READY
            if (strcmp (zmsg_address (zmsg), "READY") == 0)
                zmsg_destroy (&zmsg);
            else
                zmsg_send (&zmsg, frontend);
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
    //  We never exit the main loop
    return 0;
}
