//
//  Broker peering simulation (part 3)
//  Prototypes the full flow of status and tasks
//
//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each thread has its own
//  context and conceptually acts as a separate process.
//
//  Changes for 2.1:
//  - added version assertion
//  - use separate contexts for each thread
//  - use ipc:// instead of inproc://
//  - close sockets in each child thread
//  - call zmq_term in each thread before ending

#include "zmsg.h"

#define NBR_CLIENTS 10
#define NBR_WORKERS 5

//  Dequeue operation for queue implemented as array of anything
#define DEQUEUE(q) memmove (&(q)[0], &(q)[1], sizeof (q) - sizeof (q [0]))

//  Our own name; in practice this'd be configured per node
static char *self;

//  Request-reply client using REQ socket
//  To simulate load, clients issue a burst of requests and then
//  sleep for a random period.
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

    void *monitor = zmq_socket (context, ZMQ_PUSH);
    snprintf (endpoint, 255, "ipc://%s-monitor.ipc", self);
    rc = zmq_connect (monitor, endpoint);
    assert (rc == 0);

    while (1) {
        sleep (randof (5));

        int burst = randof (15);
        while (burst--) {
            //  Send request with random hex ID
            char task_id [5];
            sprintf (task_id, "%04X", randof (0x10000));
            zmsg_t *zmsg = zmsg_new (task_id);
            zmsg_send (&zmsg, client);

            //  Wait max ten seconds for a reply, then complain
            zmq_pollitem_t pollset [1] = { { client, 0, ZMQ_POLLIN, 0 } };
            rc = zmq_poll (pollset, 1, 10 * 1000000);
            assert (rc >= 0);

            if (pollset [0].revents & ZMQ_POLLIN) {
                zmsg_t *zmsg = zmsg_recv (client);
                //  Worker is supposed to answer us with our task id
                assert (streq (zmsg_body (zmsg), task_id));
                zmsg_destroy (&zmsg);
            }
            else {
                zmsg_t *zmsg = zmsg_new (NULL);
                zmsg_body_fmt (zmsg,
                    "E: CLIENT EXIT - lost task %s", task_id);
                zmsg_send (&zmsg, monitor);
                return NULL;
            }
        }
    }
    //  We never get here but if we did, this is how we'd exit cleanly
    zmq_close (client);
    zmq_close (monitor);
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
        //  Workers are busy for 0/1/2 seconds
        zmsg = zmsg_recv (worker);
        sleep (randof (2));
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
    s_version_assert (2, 1);
    if (argc < 2) {
        printf ("syntax: peering3 me {you}...\n");
        exit (EXIT_FAILURE);
    }
    self = argv [1];
    printf ("I: preparing broker at %s...\n", self);
    srandom ((unsigned) time (NULL));

    //  Prepare our context and sockets
    void *context = zmq_init (1);
    char endpoint [256];

    //  Bind cloud frontend to endpoint
    void *cloudfe = zmq_socket (context, ZMQ_XREP);
    snprintf (endpoint, 255, "ipc://%s-cloud.ipc", self);
    zmq_setsockopt (cloudfe, ZMQ_IDENTITY, self, strlen (self));
    int rc = zmq_bind (cloudfe, endpoint);
    assert (rc == 0);

    //  Bind state backend / publisher to endpoint
    void *statebe = zmq_socket (context, ZMQ_PUB);
    snprintf (endpoint, 255, "ipc://%s-state.ipc", self);
    rc = zmq_bind (statebe, endpoint);
    assert (rc == 0);

    //  Connect cloud backend to all peers
    void *cloudbe = zmq_socket (context, ZMQ_XREP);
    zmq_setsockopt (cloudbe, ZMQ_IDENTITY, self, strlen (self));

    int argn;
    for (argn = 2; argn < argc; argn++) {
        char *peer = argv [argn];
        printf ("I: connecting to cloud frontend at '%s'\n", peer);
        snprintf (endpoint, 255, "ipc://%s-cloud.ipc", peer);
        rc = zmq_connect (cloudbe, endpoint);
        assert (rc == 0);
    }

    //  Connect statefe to all peers
    void *statefe = zmq_socket (context, ZMQ_SUB);
    zmq_setsockopt (statefe, ZMQ_SUBSCRIBE, "", 0);

    for (argn = 2; argn < argc; argn++) {
        char *peer = argv [argn];
        printf ("I: connecting to state backend at '%s'\n", peer);
        snprintf (endpoint, 255, "ipc://%s-state.ipc", peer);
        rc = zmq_connect (statefe, endpoint);
        assert (rc == 0);
    }
    //  Prepare local frontend and backend
    void *localfe = zmq_socket (context, ZMQ_XREP);
    snprintf (endpoint, 255, "ipc://%s-localfe.ipc", self);
    rc = zmq_bind (localfe, endpoint);
    assert (rc == 0);

    void *localbe = zmq_socket (context, ZMQ_XREP);
    snprintf (endpoint, 255, "ipc://%s-localbe.ipc", self);
    rc = zmq_bind (localbe, endpoint);
    assert (rc == 0);

    //  Prepare monitor socket
    void *monitor = zmq_socket (context, ZMQ_PULL);
    snprintf (endpoint, 255, "ipc://%s-monitor.ipc", self);
    rc = zmq_bind (monitor, endpoint);
    assert (rc == 0);

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
        rc = zmq_poll (primary, 4, local_capacity? 1000000: -1);
        assert (rc >= 0);

        //  Track if capacity changes during this iteration
        int previous = local_capacity;

        //  Handle reply from local worker
        zmsg_t *zmsg = NULL;

        if (primary [0].revents & ZMQ_POLLIN) {
            assert (local_capacity < NBR_WORKERS);
            //  Use worker address for LRU routing
            zmsg = zmsg_recv (localbe);
            worker_queue [local_capacity++] = zmsg_unwrap (zmsg);
            if (streq (zmsg_address (zmsg), "READY"))
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
            if (streq (zmsg_address (zmsg), argv [argn]))
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
                rc = zmq_poll (secondary, 2, 0);
            else
                rc = zmq_poll (secondary, 1, 0);
            assert (rc >= 0);

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
                int random_peer = randof (argc - 2) + 2;
                zmsg_wrap (zmsg, argv [random_peer], NULL);
                zmsg_send (&zmsg, cloudbe);
            }
        }
        if (local_capacity != previous) {
            //  Broadcast new capacity
            zmsg_t *zmsg = zmsg_new (NULL);
            zmsg_body_fmt (zmsg, "%d", local_capacity);
            //  We stick our own address onto the envelope
            zmsg_wrap (zmsg, self, NULL);
            zmsg_send (&zmsg, statebe);
        }
    }
    //  We never get here but clean up anyhow
    zmq_close (localbe);
    zmq_close (cloudbe);
    zmq_close (statefe);
    zmq_close (monitor);
    zmq_term (context);
    return EXIT_SUCCESS;
}
