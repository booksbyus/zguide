//  Asynchronous client-to-server (DEALER to ROUTER)
//
//  While this example runs in a single process, that is to make
//  it easier to start and stop the example. Each task conceptually
//  acts as a separate process.

#include "czmq.h"

//  This is our client task
//  It connects to the server, and then sends a request once per second
//  It collects responses as they arrive, and it prints them out. We will
//  run several client tasks in parallel, each with a different random ID.

static void
client_task (zsock_t *pipe, void *args)
{
    zsock_signal(pipe, 0);

    zsock_t *client = zsock_new (ZMQ_DEALER);

    //  Set random identity to make tracing easier (must be done before zsock_connect)
    char identity [10];
    sprintf (identity, "%04X-%04X", randof (0x10000), randof (0x10000));
    zsock_set_identity (client, identity);
    zsock_connect (client, "tcp://localhost:5570");

    zpoller_t *poller = zpoller_new (pipe, client, NULL);
    zpoller_set_nonstop(poller, true);
    
    bool signaled = false;
    int request_nbr = 0;
    while (!signaled) {
        //  Tick once per second, pulling in arriving messages
        int centitick;
        for (centitick = 0; centitick < 100; centitick++) {
            zsock_t *ready = zpoller_wait(poller, 10 * ZMQ_POLL_MSEC);
            if (ready == NULL) continue;
            else if  (ready == pipe) {
                signaled = true;
                break;
            } else assert (ready == client);

            zmsg_t *msg = zmsg_recv (client);
            zframe_print (zmsg_last (msg), identity);
            zmsg_destroy (&msg);
        }
        zstr_sendf (client, "request #%d", ++request_nbr);
    }
    zpoller_destroy(&poller);
    zsock_destroy(&client);
}

//  .split server task
//  This is our server task.
//  It uses the multithreaded server model to deal requests out to a pool
//  of workers and route replies back to clients. One worker can handle
//  one request at a time but one client can talk to multiple workers at
//  once.

static void server_worker (zsock_t *pipe, void *args);

static void server_task (zsock_t *pipe, void *args)
{
    zsock_signal(pipe, 0);
    
    //  Launch pool of worker threads, precise number is not critical
    enum { NBR_THREADS = 5 };
    zactor_t *threads[NBR_THREADS];
    int thread_nbr;
    for (thread_nbr = 0; thread_nbr < NBR_THREADS; thread_nbr++)
        threads[thread_nbr] = zactor_new (server_worker, NULL);

    //  Connect backend to frontend via a zproxy
    zactor_t *proxy = zactor_new (zproxy, NULL);
    zstr_sendx (proxy, "FRONTEND", "ROUTER", "tcp://*:5570", NULL);
    zsock_wait (proxy);

    zstr_sendx (proxy, "BACKEND", "DEALER", "inproc://backend", NULL);
    zsock_wait (proxy);

    // Wait for shutdown signal
    zsock_wait(pipe);
    zactor_destroy(&proxy);
    
    for (thread_nbr = 0; thread_nbr < NBR_THREADS; thread_nbr++)
        zactor_destroy(&threads[thread_nbr]);
}

//  .split worker task
//  Each worker task works on one request at a time and sends a random number
//  of replies back, with random delays between replies:

static void
server_worker (zsock_t *pipe, void *args)
{
    zsock_signal(pipe, 0);
    
    zsock_t *worker = zsock_new_dealer ("inproc://backend");

    zpoller_t *poller = zpoller_new (pipe, worker, NULL);
    zpoller_set_nonstop (poller, true);
    
    while (true) {
        zsock_t *ready = zpoller_wait (poller, -1);
        if (ready == NULL) continue;
        else if (ready == pipe) break;
        else assert (ready == worker);
        
        //  The DEALER socket gives us the reply envelope and message
        zmsg_t *msg = zmsg_recv (worker);
        zframe_t *identity = zmsg_pop (msg);
        zframe_t *content = zmsg_pop (msg);
        assert (content);
        zmsg_destroy (&msg);

        //  Send 0..4 replies back
        int reply, replies = randof (5);
        for (reply = 0; reply < replies; reply++) {
            //  Sleep for some fraction of a second
            zclock_sleep (randof (1000) + 1);
            zframe_send (&identity, worker, ZFRAME_REUSE | ZFRAME_MORE | ZFRAME_DONTWAIT );
            zframe_send (&content, worker, ZFRAME_REUSE | ZFRAME_DONTWAIT );
        }
        zframe_destroy (&identity);
        zframe_destroy (&content);
    }

    zpoller_destroy (&poller);
    zsock_destroy (&worker);
}

//  The main thread simply starts several clients and a server, and then
//  waits for the server to finish.

int main (void)
{
    zactor_t *client1 = zactor_new (client_task, NULL);
    zactor_t *client2 = zactor_new (client_task, NULL);
    zactor_t *client3 = zactor_new (client_task, NULL);
    zactor_t *server = zactor_new (server_task, NULL);

    zclock_sleep (5 * 1000);    //  Run for 5 seconds then quit
    zsock_signal (server, 0);

    zactor_destroy (&server);
    zactor_destroy (&client1);
    zactor_destroy (&client2);
    zactor_destroy (&client3);
    return 0;
}
