//
//  Asynchronous client-to-server (XREQ to XRER)
//
//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each task has its own
//  context and conceptually acts as a separate process.

#include "zhelpers.h"
#include "zmsg.c"

//  ---------------------------------------------------------------------
//  This is our client task
//  It connects to the server, and then sends a request once per second
//  It collects responses as they arrive, and it prints them out. We will
//  run several client tasks in parallel, each with a different random ID.

static void *
client_task (void *args) {
    void *context = zmq_init (1);
    void *client = zmq_socket (context, ZMQ_XREQ);
    zmq_connect (client, "tcp://localhost:5570");

    //  Generate printable identity for the client
    char identity [5];
    sprintf (identity, "%04X", randof (0x10000));
    zmq_setsockopt (client, ZMQ_IDENTITY, identity, strlen (identity));

    zmq_pollitem_t items [] = { { client, 0, ZMQ_POLLIN, 0 } };
    int request_nbr = 0;
    while (1) {
        //  Tick once per second, pulling in arriving messages
        int centitick;
        for (centitick = 0; centitick < 100; centitick++) {
            zmq_poll (items, 1, 10000);
            if (items [0].revents & ZMQ_POLLIN) {
                zmsg_t *zmsg = zmsg_recv (client);
                printf ("%s: %s\n", identity, zmsg_body (zmsg));
                zmsg_destroy (&zmsg);
            }
        }
        zmsg_t *zmsg = zmsg_new ();
        zmsg_body_fmt (zmsg, "request #%d", ++request_nbr);
        zmsg_send (&zmsg, client);
    }
    //  Clean up and end task properly
    zmq_close (client);
    zmq_term (context);
    return (NULL);
}

//  ---------------------------------------------------------------------
//  This is our server task
//  It uses the multithreaded server model to deal requests out to a pool
//  of workers and route replies back to clients. One worker can handle
//  one request at a time but one client can talk to multiple workers at
//  once.

static void *server_worker (void *socket);

void *server_task (void *args) {
    void *context = zmq_init (1);
    
    //  Frontend socket talks to clients over TCP
    void *frontend = zmq_socket (context, ZMQ_XREQ);
    zmq_bind (frontend, "tcp://*:5570");

    //  Backend socket talks to workers over inproc
    void *backend = zmq_socket (context, ZMQ_XREQ);
    zmq_bind (backend, "inproc://backend");

    //  Launch pool of worker threads, precise number is not critical
    int thread_nbr;
    for (thread_nbr = 0; thread_nbr < 5; thread_nbr++) {
        pthread_t worker_thread;
        pthread_create (&worker_thread, NULL, server_worker, context);
    }
    //  Connect backend to frontend via an inprocess queue device
    zmq_device (ZMQ_QUEUE, frontend, backend);

    zmq_close (frontend);
    zmq_close (backend);
    zmq_term (context);
    return (NULL);
}

//  Accept a request and reply with the same text a random number of
//  times, with random delays between replies.
//
static void *
server_worker (void *context) {
    void *worker = zmq_socket (context, ZMQ_XREQ);
    zmq_connect (worker, "inproc://backend");

    while (1) {
        //  The XREQ socket gives us a message (without envelope)
        char *message = s_recv (worker);
        //  Send 0..4 replies back
        int reply, replies = randof (5);
        for (reply = 0; reply < replies; reply++) {
            //  Sleep for some fraction of a second
            struct timespec t = { 0, randof (100000000) + 1 };
            nanosleep (&t, NULL);
            s_send (worker, message);
        }
        free (message);
    }
    zmq_close (worker);
    return (NULL);
}


//  This main thread simply starts several clients, and a server, and then
//  waits for the server to finish.
//
int main () {
    s_version_assert (2, 1);

    pthread_t client_thread;
    pthread_create (&client_thread, NULL, client_task, NULL);
    pthread_create (&client_thread, NULL, client_task, NULL);
    pthread_create (&client_thread, NULL, client_task, NULL);

    pthread_t server_thread;
    pthread_create (&server_thread, NULL, server_task, NULL);
    pthread_join (server_thread, NULL);
    return 0;
}
