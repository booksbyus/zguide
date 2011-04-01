//
//  Broker peering simulation (part 2)
//  Prototypes the request-reply flow
//
//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each thread has its own
//  context and conceptually acts as a separate process.
//
#include "zmsg.h"

#define NBR_CLIENTS 10
#define NBR_WORKERS 3

//  Dequeue operation for queue implemented as array of anything
#define DEQUEUE(q) memmove (&(q)[0], &(q)[1], sizeof (q) - sizeof (q [0]))

//  Our own name; in practice this'd be configured per node
static char *self;

//  Request-reply client using REQ socket
//
static void *
client_task (void *args)
{
    void *context = zmq_init (1);

    void *client = zmq_socket (context, ZMQ_REQ);
    char endpoint [256];
    snprintf (endpoint, 255, "ipc://%s-localfe.ipc", self);
    int rc = zmq_connect (client, endpoint);
    assert (rc == 0);

    while (1) {
        //  Send request, get reply
        zmsg_t *zmsg = zmsg_new ("HELLO");
        zmsg_send (&zmsg, client);
        zmsg = zmsg_recv (client);
        printf ("I: client status: %s\n", zmsg_body (zmsg));
        zmsg_destroy (&zmsg);
    }
    //  We never get here but if we did, this is how we'd exit cleanly
    zmq_close (client);
    zmq_term (context);
    return NULL;
}

//  Worker using REQ socket to do LRU routing
//
static void *
worker_task (void *args)
{
    void *context = zmq_init (1);

    void *worker = zmq_socket (context, ZMQ_REQ);
    char endpoint [256];
    snprintf (endpoint, 255, "ipc://%s-localbe.ipc", self);
    int rc = zmq_connect (worker, endpoint);
    assert (rc == 0);

    //  Tell broker we're ready for work
    zmsg_t *zmsg = zmsg_new ("READY");
    zmsg_send (&zmsg, worker);

    while (1) {
        zmsg = zmsg_recv (worker);
        //  Do some 'work'
        sleep (1);
        zmsg_body_fmt (zmsg, "OK - %04x", randof (0x10000));
        zmsg_send (&zmsg, worker);
    }
    //  We never get here but if we did, this is how we'd exit cleanly
    zmq_close (worker);
    zmq_term (context);
    return NULL;
}


int main (int argc, char *argv [])
{
    //  First argument is this broker's name
    //  Other arguments are our peers' names
    //
    if (argc < 2) {
        printf ("syntax: peering2 me {you}...\n");
        exit (EXIT_FAILURE);
    }
    self = argv [1];
    printf ("I: preparing broker at %s...\n", self);
    srandom ((unsigned) time (NULL));

    //  Prepare our context and sockets
    void *context = zmq_init (1);
    char endpoint [256];

    //  Bind cloud frontend to endpoint
    void *cloudfe = zmq_socket (context, ZMQ_ROUTER);
    snprintf (endpoint, 255, "ipc://%s-cloud.ipc", self);
    zmq_setsockopt (cloudfe, ZMQ_IDENTITY, self, strlen (self));
    int rc = zmq_bind (cloudfe, endpoint);
    assert (rc == 0);

    //  Connect cloud backend to all peers
    void *cloudbe = zmq_socket (context, ZMQ_ROUTER);
    zmq_setsockopt (cloudbe, ZMQ_IDENTITY, self, strlen (self));

    int argn;
    for (argn = 2; argn < argc; argn++) {
        char *peer = argv [argn];
        printf ("I: connecting to cloud frontend at '%s'\n", peer);
        snprintf (endpoint, 255, "ipc://%s-cloud.ipc", peer);
        rc = zmq_connect (cloudbe, endpoint);
        assert (rc == 0);
    }

    //  Prepare local frontend and backend
    void *localfe = zmq_socket (context, ZMQ_ROUTER);
    snprintf (endpoint, 255, "ipc://%s-localfe.ipc", self);
    rc = zmq_bind (localfe, endpoint);
    assert (rc == 0);

    void *localbe = zmq_socket (context, ZMQ_ROUTER);
    snprintf (endpoint, 255, "ipc://%s-localbe.ipc", self);
    rc = zmq_bind (localbe, endpoint);
    assert (rc == 0);

    //  Get user to tell us when we can start...
    printf ("Press Enter when all brokers are started: ");
    getchar ();

    //  Start local workers
    int worker_nbr;
    for (worker_nbr = 0; worker_nbr < NBR_WORKERS; worker_nbr++) {
        pthread_t worker;
        pthread_create (&worker, NULL, worker_task, NULL);
    }
    //  Start local clients
    int client_nbr;
    for (client_nbr = 0; client_nbr < NBR_CLIENTS; client_nbr++) {
        pthread_t client;
        pthread_create (&client, NULL, client_task, NULL);
    }

    //  Interesting part
    //  -------------------------------------------------------------
    //  Request-reply flow
    //  - Poll backends and process local/cloud replies
    //  - While worker available, route localfe to local or cloud

    //  Queue of available workers
    int capacity = 0;
    char *worker_queue [NBR_WORKERS];

    while (1) {
        zmq_pollitem_t backends [] = {
            { localbe, 0, ZMQ_POLLIN, 0 },
            { cloudbe, 0, ZMQ_POLLIN, 0 }
        };
        //  If we have no workers anyhow, wait indefinitely
        rc = zmq_poll (backends, 2, capacity? 1000000: -1);
        assert (rc >= 0);

        //  Handle reply from local worker
        zmsg_t *zmsg = NULL;
        if (backends [0].revents & ZMQ_POLLIN) {
            zmsg = zmsg_recv (localbe);

            assert (capacity < NBR_WORKERS);
            //  Use worker address for LRU routing
            worker_queue [capacity++] = zmsg_unwrap (zmsg);
            if (streq (zmsg_address (zmsg), "READY"))
                zmsg_destroy (&zmsg);   //  Don't route it
        }
        //  Or handle reply from peer broker
        else
        if (backends [1].revents & ZMQ_POLLIN) {
            zmsg = zmsg_recv (cloudbe);
            //  We don't use peer broker address for anything
            free (zmsg_unwrap (zmsg));
        }
        //  Route reply to cloud if it's addressed to a broker
        for (argn = 2; zmsg && argn < argc; argn++) {
            if (streq (zmsg_address (zmsg), argv [argn]))
                zmsg_send (&zmsg, cloudfe);
        }
        //  Route reply to client if we still need to
        if (zmsg)
            zmsg_send (&zmsg, localfe);

        //  Now route as many clients requests as we can handle
        //
        while (capacity) {
            zmq_pollitem_t frontends [] = {
                { localfe, 0, ZMQ_POLLIN, 0 },
                { cloudfe, 0, ZMQ_POLLIN, 0 }
            };
            rc = zmq_poll (frontends, 2, 0);
            assert (rc >= 0);
            int reroutable = 0;
            //  We'll do peer brokers first, to prevent starvation
            if (frontends [1].revents & ZMQ_POLLIN) {
                zmsg = zmsg_recv (cloudfe);
                reroutable = 0;
            }
            else
            if (frontends [0].revents & ZMQ_POLLIN) {
                zmsg = zmsg_recv (localfe);
                reroutable = 1;
            }
            else
                break;      //  No work, go back to backends

            //  If reroutable, send to cloud 20% of the time
            //  Here we'd normally use cloud status information
            //
            if (reroutable && argc > 2 && randof (5) == 0) {
                //  Route to random broker peer
                int random_peer = randof (argc - 2) + 2;
                zmsg_wrap (zmsg, argv [random_peer], NULL);
                zmsg_send (&zmsg, cloudbe);
            }
            else {
                zmsg_wrap (zmsg, worker_queue [0], "");
                zmsg_send (&zmsg, localbe);

                //  Dequeue and drop the next worker address
                free (worker_queue [0]);
                DEQUEUE (worker_queue);
                capacity--;
            }
        }
    }
    //  We never get here but clean up anyhow
    zmq_close (localbe);
    zmq_close (cloudbe);
    zmq_term (context);
    return EXIT_SUCCESS;
}
