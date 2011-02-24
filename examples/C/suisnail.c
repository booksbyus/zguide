//
//  Suicidal Snail
//
#include "zhelpers.h"

//  ---------------------------------------------------------------------
//  This is our subscriber
//  It connects to the publisher and subscribes to everything. It 
//  sleeps for a short time between messages to simulate doing too
//  much work. If a message is more than 1 second late, it croaks.

#define MAX_ALLOWED_DELAY   1000    //  msecs

static void *
subscriber (void *args) {
    void *context = zmq_init (1);

    //  Subscribe to everything
    void *subscriber = zmq_socket (context, ZMQ_SUB);
    zmq_connect (subscriber, "tcp://localhost:5556");
    zmq_setsockopt (subscriber, ZMQ_SUBSCRIBE, "", 0);

    //  Get and process messages
    while (1) {
        char *string = s_recv (subscriber);
        int64_t clock;
        int terms = sscanf (string, "%" PRId64, &clock);
        assert (terms == 1);
        free (string);
        
        //  Suicide snail logic
        if (s_clock () - clock > MAX_ALLOWED_DELAY) {
            fprintf (stderr, "E: subscriber cannot keep up, aborting\n");
            break;
        }
        //  Work for 1 msec plus some random additional time
        s_sleep (1 + randof (2));
    }
    zmq_close (subscriber);
    zmq_term (context);
    return (NULL);
}


//  ---------------------------------------------------------------------
//  This is our server task
//  It publishes a time-stamped message to its pub socket every 1ms.

static void *
publisher (void *args) {
    void *context = zmq_init (1);

    //  Prepare publisher
    void *publisher = zmq_socket (context, ZMQ_PUB);
    zmq_bind (publisher, "tcp://*:5556");

    while (1) {
        //  Send current clock (msecs) to subscribers
        char string [20];
        sprintf (string, "%" PRId64, s_clock ());
        s_send (publisher, string);
        s_sleep (1);            //  1msec wait
    }
    zmq_close (publisher);
    zmq_term (context);
    return (NULL);
}


//  This main thread simply starts a client, and a server, and then
//  waits for the client to croak.
//
int main (void) {
    pthread_t server_thread;
    pthread_create (&server_thread, NULL, publisher, NULL);
    
    pthread_t client_thread;
    pthread_create (&client_thread, NULL, subscriber, NULL);
    pthread_join (client_thread, NULL);

    return 0;
}
