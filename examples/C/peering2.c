//
//  Custom routing Router to Router (XREP to XREP)
//  Data center simulation, inproc: within process
//  and inproc: between processes.
//
#include "zhelpers.h"
#include "zmsg.c"

#define NBR_CLIENTS 1
#define NBR_WORKERS 2

//  A simple dequeue operation for queue implemented as array
#define DEQUEUE(q) memmove (&(q)[0], &(q)[1], sizeof (q) - sizeof (q [0]))

//  Request-reply client using REQ socket
//
static void *
client_thread (void *context) {
    void *client = zmq_socket (context, ZMQ_REQ);
    zmq_connect (client, "inproc://localfe");

    zmsg_t *zmsg = zmsg_new ();
    while (1) {
        //  Send request, get reply
        zmsg_body_set (zmsg, "HELLO");
        puts ("** client send"); fflush (stdout);
        zmsg_send (&zmsg, client);
        zmsg = zmsg_recv (client);
        puts ("** client recv"); fflush (stdout);
    }
    return (NULL);
}

//  Worker using REQ socket to do LRU routing
//
static void *
worker_thread (void *context) {
    void *worker = zmq_socket (context, ZMQ_REQ);
    zmq_connect (worker, "inproc://localbe");

    //  Tell broker we're ready for work
    zmsg_t *zmsg = zmsg_new ();
    zmsg_body_set (zmsg, "READY");
    puts ("** worker send"); fflush (stdout);
    zmsg_send (&zmsg, worker);

    while (1) {
        zmsg = zmsg_recv (worker);
        puts ("** worker recv"); fflush (stdout);
        //  Do some 'work'
        sleep (1);
        zmsg_body_set (zmsg, "OK");
        puts ("** worker send"); fflush (stdout);
        zmsg_send (&zmsg, worker);
    }
    return (NULL);
}


int main (int argc, char *argv[])
{
    //  First argument is this cluster's name
    //  Other arguments are our peers' names
    //
    if (argc < 3) {
        printf ("syntax: peering1 me other1 other2...\n");
        exit (EXIT_FAILURE);
    }
    char *self = argv [1];
    printf ("I: preparing broker at %s...\n", self);
    printf ("Press Enter when all brokers are started: ");
    getchar ();

    //  Prepare our context and sockets
    void *context = zmq_init (1);
    char endpoint [256];

    //  Bind cloud frontend to endpoint
    void *cloudfe = zmq_socket (context, ZMQ_XREP);
    snprintf (endpoint, 255, "ipc://%s-cloud.ipc", self);
    zmq_setsockopt (cloudfe, ZMQ_IDENTITY, self, strlen (self));
    assert (zmq_bind (cloudfe, endpoint) == 0);

    //  Connect cloud backend to all peers
    int argn;
    void *cloudbe = zmq_socket (context, ZMQ_XREP);
    zmq_setsockopt (cloudbe, ZMQ_IDENTITY, self, strlen (self));
    for (argn = 2; argn < argc; argn++) {
        char *peer = argv [argn];
        printf ("I: peering with broker at '%s'\n", peer);
        snprintf (endpoint, 255, "ipc://%s-cloud.ipc", peer);
        assert (zmq_connect (cloudbe, endpoint) == 0);
    }
    //  Prepare local frontend and backend
    void *localfe = zmq_socket (context, ZMQ_XREP);
    zmq_bind (localfe, "inproc://localfe");
    void *localbe = zmq_socket (context, ZMQ_XREP);
    zmq_bind (localbe, "inproc://localbe");

    //  Start local clients and local workers
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

    //  Interesting part
    //  -------------------------------------------------------------
    //  Request-reply flow
    //  - Poll backends and process local/cloud replies
    //  - While worker available, route localfe to local or cloud

    //  Queue of available workers
    int available_workers = 0;
    char *worker_queue [NBR_WORKERS];

    while (1) {
        zmq_pollitem_t backends [] = {
            { localbe, 0, ZMQ_POLLIN, 0 },
            { cloudbe, 0, ZMQ_POLLIN, 0 }
        };
        //  If we have no workers anyhow, wait indefinitely
        zmq_poll (backends, 2, available_workers? 0: -1);
        zmsg_t *zmsg;

        //  Handle reply from local worker
        if (backends [0].revents & ZMQ_POLLIN) {
            zmsg = zmsg_recv (localbe);
            puts ("** localbe recv");

            assert (available_workers < NBR_WORKERS);
            //  Use worker address for LRU routing
            worker_queue [available_workers++] = zmsg_unwrap (zmsg);
            if (strcmp (zmsg_address (zmsg), "READY") == 0)
                zmsg_destroy (&zmsg);   //  Don't route it
        }
        //  Handle reply from peer broker
        if (backends [1].revents & ZMQ_POLLIN) {
            zmsg = zmsg_recv (cloudbe);
            puts ("** cloudbe recv");
            //  We don't use peer broker address for anything
            free (zmsg_unwrap (zmsg));
        }
        //  Route reply to cloud if it's addressed to a broker
        for (argn = 2; zmsg && argn < argc; argn++) {
            if (strcmp (zmsg_address (zmsg), argv [argn]) == 0) {
                puts ("** cloudfe send");
                zmsg_send (&zmsg, cloudfe);
            }
        }
        //  Route reply to client if we still need to
        if (zmsg) {
            puts ("** localfe send");
            zmsg_send (&zmsg, localfe);
        }
        //  Now route as many clients requests as we can handle
        while (available_workers) {
            zmq_pollitem_t frontends [] = {
                { localfe, 0, ZMQ_POLLIN, 0 },
                { cloudfe, 0, ZMQ_POLLIN, 0 }
            };
            zmq_poll (frontends, 2, 0);
            int reroutable = 0;
            //  We'll do peer brokers first, to prevent starvation
            if (frontends [1].revents & ZMQ_POLLIN) {
                zmsg = zmsg_recv (cloudfe);
                puts ("** cloudfe recv");
                reroutable = 0;
            }
            else
            if (frontends [0].revents & ZMQ_POLLIN) {
                zmsg = zmsg_recv (localfe);
                puts ("** localfe recv");
                reroutable = 1;
            }
            else
                break;      //  No work, go back to backends

            //  If reroutable, send to cloud if possible
            //  Here we'd normally use cloud status information
            if (reroutable && within (NBR_WORKERS) == 0) {
                //  Route to random broker peer
                int random_peer = within (argc - 2) + 2;
                zmsg_wrap (zmsg, argv [random_peer], NULL);
                puts ("** cloudbe send");
                zmsg_send (&zmsg, cloudbe);
            }
            else {
                zmsg_wrap (zmsg, worker_queue [0], "");
                puts ("** localbe send");
                zmsg_send (&zmsg, localbe);

                //  Dequeue and drop the next worker address
                free (worker_queue [0]);
                DEQUEUE (worker_queue);
                available_workers--;
            }
        }
    }
    zmq_term (context);
    return 0;
}
