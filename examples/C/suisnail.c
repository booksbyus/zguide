//
//  Suicidal Snail
//
#include "zapi.h"

//  ---------------------------------------------------------------------
//  This is our subscriber
//  It connects to the publisher and subscribes to everything. It
//  sleeps for a short time between messages to simulate doing too
//  much work. If a message is more than 1 second late, it croaks.

#define MAX_ALLOWED_DELAY   1000    //  msecs

static void *
subscriber (void *args) {
    zctx_t *ctx = zctx_new ();

    //  Subscribe to everything
    void *subscriber = zctx_socket_new (ctx, ZMQ_SUB);
    zmq_connect (subscriber, "tcp://localhost:5556");
    zmq_setsockopt (subscriber, ZMQ_SUBSCRIBE, "", 0);

    //  Get and process messages
    while (1) {
        char *string = zstr_recv (subscriber);
        int64_t clock;
        int terms = sscanf (string, "%" PRId64, &clock);
        assert (terms == 1);
        free (string);

        //  Suicide snail logic
        if (zclock_time () - clock > MAX_ALLOWED_DELAY) {
            fprintf (stderr, "E: subscriber cannot keep up, aborting\n");
            break;
        }
        //  Work for 1 msec plus some random additional time
        zclock_sleep (1 + randof (2));
    }
    zctx_destroy (&ctx);
    puts ("THERE");
    zstr_send (((zthread_t *) args)->pipe, "gone and died");
    return NULL;
}


//  ---------------------------------------------------------------------
//  This is our server task
//  It publishes a time-stamped message to its pub socket every 1ms.

static void *
publisher (void *args) {
    zctx_t *ctx = zctx_new ();

    //  Prepare publisher
    void *publisher = zctx_socket_new (ctx, ZMQ_PUB);
    zmq_bind (publisher, "tcp://*:5556");

    while (1) {
        //  Send current clock (msecs) to subscribers
        char string [20];
        sprintf (string, "%" PRId64, zclock_time ());
        zstr_send (publisher, string);
        char *signal = zstr_recv_nowait (((zthread_t *) args)->pipe);
        if (signal) {
    puts ("WOWEE");
            free (signal);
            break;
        }
        zclock_sleep (1);            //  1msec wait
    }
    puts ("ZOWEE");
    zctx_destroy (&ctx);
    return NULL;
}


//  This main thread simply starts a client, and a server, and then
//  waits for the client to signal it's died.
//
int main (void)
{
    zctx_t *ctx = zctx_new ();
    void *pubpipe = zctx_thread_new (ctx, publisher, NULL);
    void *subpipe = zctx_thread_new (ctx, subscriber, NULL);
    free (zstr_recv (subpipe));
    puts ("HERE");
    zstr_send (pubpipe, "break");
    zclock_sleep (100);
    zctx_destroy (&ctx);
    puts ("WHERE?");
    return 0;
}
