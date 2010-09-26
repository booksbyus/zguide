//
//  Broker peering simulation (part 3)
//  Prototypes the full flow of status and tasks
//
#include "zhelpers.h"
#include "zmsg.c"

#define NBR_CLIENTS 10
#define NBR_WORKERS 5

//  A simple dequeue operation for queue implemented as array
#define DEQUEUE(q) memmove (&(q)[0], &(q)[1], sizeof (q) - sizeof (q [0]))

//  Request-reply client using REQ socket
//  To simulate load, clients issue a burst of requests and then
//  sleep for a random period.
//
static void *
client_thread (void *context) {
    void *client = zmq_socket (context, ZMQ_REQ);
    assert (zmq_connect (client, "inproc://localfe") == 0);
    void *monitor = zmq_socket (context, ZMQ_PUSH);
    assert (zmq_connect (monitor, "inproc://monitor") == 0);

    zmsg_t *zmsg = zmsg_new ();
    while (1) {
        sleep (within (5));
        int burst = within (15);
        while (burst--) {
            //  Send request with random hex ID
            char task_id [5];
            sprintf (task_id, "%04X", within (0x10000));
            zmsg_body_set (zmsg, task_id);
            zmsg_send (&zmsg, client);

            //  Wait max ten seconds for a reply, then complain
            zmq_pollitem_t pollset [1] = {
                { client, 0, ZMQ_POLLIN, 0 }
            };
            assert (zmq_poll (pollset, 1, 10 * 1000000) >= 0);
            if (pollset [0].revents & ZMQ_POLLIN) {
                zmsg = zmsg_recv (client);
                //  Worker is supposed to answer us with our task id
                assert (strcmp (zmsg_body (zmsg), task_id) == 0);
            }
            else {
                zmsg = zmsg_new ();
                zmsg_body_fmt (zmsg,
                    "E: CLIENT EXIT - lost task %s", task_id);
                zmsg_send (&zmsg, monitor);
                return (NULL);
            }
        }
    }
    return (NULL);
}

//  Worker using REQ socket to do LRU routing
//
static void *
worker_thread (void *context) {
    void *worker = zmq_socket (context, ZMQ_REQ);
    assert (zmq_connect (worker, "inproc://localbe") == 0);

    //  Tell broker we're ready for work
    zmsg_t *zmsg = zmsg_new ();
    zmsg_body_set (zmsg, "READY");
    zmsg_send (&zmsg, worker);

    while (1) {
        //  Workers are busy for 0/1/2 seconds
        zmsg = zmsg_recv (worker);
        sleep (within (2));
        zmsg_send (&zmsg, worker);
    }
    return (NULL);
}

int main (int argc, char *argv[])
{
    //  First argument is this broker's name
    //  Other arguments are our peers' names
    //
    s_version ();
    if (argc < 2) {
        printf ("syntax: peering3 me {you}...\n");
        exit (EXIT_FAILURE);
    }
    char *self = argv [1];
    printf ("I: preparing broker at %s...\n", self);
    srandom ((unsigned) time (NULL));

    //  Prepare our context and sockets
    void *context = zmq_init (1);
    char endpoint [256];

    //  Bind cloud frontend to endpoint
    void *cloudfe = zmq_socket (context, ZMQ_XREP);
    snprintf (endpoint, 255, "ipc://%s-cloud.ipc", self);
    zmq_setsockopt (cloudfe, ZMQ_IDENTITY, self, strlen (self));
    assert (zmq_bind (cloudfe, endpoint) == 0);

    //  Bind state backend / publisher to endpoint
    void *statebe = zmq_socket (context, ZMQ_PUB);
    snprintf (endpoint, 255, "ipc://%s-state.ipc", self);
    assert (zmq_bind (statebe, endpoint) == 0);

    //  Connect cloud backend to all peers
    void *cloudbe = zmq_socket (context, ZMQ_XREP);
    zmq_setsockopt (cloudbe, ZMQ_IDENTITY, self, strlen (self));
    int argn;
    for (argn = 2; argn < argc; argn++) {
        char *peer = argv [argn];
        printf ("I: connecting to cloud frontend at '%s'\n", peer);
        snprintf (endpoint, 255, "ipc://%s-cloud.ipc", peer);
        assert (zmq_connect (cloudbe, endpoint) == 0);
    }
    //  Connect statefe to all peers
    void *statefe = zmq_socket (context, ZMQ_SUB);
    zmq_setsockopt (statefe, ZMQ_SUBSCRIBE, "", 0);
    for (argn = 2; argn < argc; argn++) {
        char *peer = argv [argn];
        printf ("I: connecting to state backend at '%s'\n", peer);
        snprintf (endpoint, 255, "ipc://%s-state.ipc", peer);
        assert (zmq_connect (statefe, endpoint) == 0);
    }
    //  Prepare local frontend and backend
    void *localfe = zmq_socket (context, ZMQ_XREP);
    zmq_bind (localfe, "inproc://localfe");
    void *localbe = zmq_socket (context, ZMQ_XREP);
    zmq_bind (localbe, "inproc://localbe");

    //  Prepare monitor socket
    void *monitor = zmq_socket (context, ZMQ_PULL);
    zmq_bind (monitor, "inproc://monitor");

    //  Start local workers
    int worker_nbr;
    for (worker_nbr = 0; worker_nbr < NBR_WORKERS; worker_nbr++) {
        pthread_t worker;
        pthread_create (&worker, NULL, worker_thread, context);
    }
    //  Start local clients
    int client_nbr;
    for (client_nbr = 0; client_nbr < NBR_CLIENTS; client_nbr++) {
        pthread_t client;
        pthread_create (&client, NULL, client_thread, context);
    }

    //  Interesting part
    //  -------------------------------------------------------------
    //  Publish-subscribe flow
    //  - Poll statefe and process capacity updates
    //  - Each time capacity changes, broadcast new value
    //  Request-reply flow
    //  - Poll primary and process local/cloud replies
    //  - While worker available, route localfe to local or cloud

    //  Queue of available workers
    int local_capacity = 0;
    int cloud_capacity = 0;
    char *worker_queue [10];

    while (1) {
        zmq_pollitem_t primary [] = {
            { localbe, 0, ZMQ_POLLIN, 0 },
            { cloudbe, 0, ZMQ_POLLIN, 0 },
            { statefe, 0, ZMQ_POLLIN, 0 },
            { monitor, 0, ZMQ_POLLIN, 0 }
        };
        //  If we have no workers anyhow, wait indefinitely
        assert (zmq_poll (primary, 4, local_capacity? 1000000: -1) >= 0);

        //  Track if capacity changes during this iteration
        int previous = local_capacity;

        //  Handle reply from local worker
        zmsg_t *zmsg = NULL;
        if (primary [0].revents & ZMQ_POLLIN) {
            assert (local_capacity < NBR_WORKERS);
            //  Use worker address for LRU routing
            zmsg = zmsg_recv (localbe);
            worker_queue [local_capacity++] = zmsg_unwrap (zmsg);
            if (strcmp (zmsg_address (zmsg), "READY") == 0)
                zmsg_destroy (&zmsg);   //  Don't route it
        }
        //  Or handle reply from peer broker
        else
        if (primary [1].revents & ZMQ_POLLIN) {
            zmsg = zmsg_recv (cloudbe);
            //  We don't use peer broker address for anything
            free (zmsg_unwrap (zmsg));
        }
        //  Route reply to cloud if it's addressed to a broker
        for (argn = 2; zmsg && argn < argc; argn++) {
            if (strcmp (zmsg_address (zmsg), argv [argn]) == 0)
                zmsg_send (&zmsg, cloudfe);
        }
        //  Route reply to client if we still need to
        if (zmsg)
            zmsg_send (&zmsg, localfe);

        //  Handle capacity updates
        if (primary [2].revents & ZMQ_POLLIN) {
            zmsg = zmsg_recv (statefe);
            cloud_capacity = atoi (zmsg_body (zmsg));
            zmsg_destroy (&zmsg);
        }
        //  Handle monitor message
        if (primary [3].revents & ZMQ_POLLIN) {
            zmsg_t *zmsg = zmsg_recv (monitor);
            printf ("%s\n", zmsg_body (zmsg));
            zmsg_destroy (&zmsg);
        }

        //  Now route as many clients requests as we can handle
        //  - If we have local capacity we poll both localfe and cloudfe
        //  - If we have cloud capacity only, we poll just localfe
        //  - Route any request locally if we can, else to cloud
        //
        while (local_capacity + cloud_capacity) {
            zmq_pollitem_t secondary [] = {
                { localfe, 0, ZMQ_POLLIN, 0 },
                { cloudfe, 0, ZMQ_POLLIN, 0 }
            };
            if (local_capacity)
                assert (zmq_poll (secondary, 2, 0) >= 0);
            else
                assert (zmq_poll (secondary, 1, 0) >= 0);

            if (secondary [0].revents & ZMQ_POLLIN)
                zmsg = zmsg_recv (localfe);
            else
            if (secondary [1].revents & ZMQ_POLLIN)
                zmsg = zmsg_recv (cloudfe);
            else
                break;      //  No work, go back to primary

            if (local_capacity) {
                zmsg_wrap (zmsg, worker_queue [0], "");
                zmsg_send (&zmsg, localbe);

                //  Dequeue and drop the next worker address
                free (worker_queue [0]);
                DEQUEUE (worker_queue);
                local_capacity--;
            }
            else {
                //  Route to random broker peer
                printf ("I: route request %s to cloud...\n",
                    zmsg_body (zmsg));
                int random_peer = within (argc - 2) + 2;
                zmsg_wrap (zmsg, argv [random_peer], NULL);
                zmsg_send (&zmsg, cloudbe);
            }
        }
        if (local_capacity != previous) {
            //  Broadcast new capacity
            zmsg_t *zmsg = zmsg_new ();
            zmsg_body_fmt (zmsg, "%d", local_capacity);
            //  We stick our own address onto the envelope
            zmsg_wrap (zmsg, self, NULL);
            zmsg_send (&zmsg, statebe);
        }
    }
    zmq_term (context);
    return EXIT_SUCCESS;
}
