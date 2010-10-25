//
//  Multithreaded relay
//
#include "zhelpers.h"

static void *
step1 (void *context) {
    //  Signal downstream to step 2
    void *sender = zmq_socket (context, ZMQ_PAIR);
    zmq_connect (sender, "inproc://step2");
    s_send (sender, "");

    return NULL;
}

static void *
step2 (void *context) {
    //  Bind to inproc: endpoint, then start upstream thread
    void *receiver = zmq_socket (context, ZMQ_PAIR);
    zmq_bind (receiver, "inproc://step2");
    pthread_t thread;
    pthread_create (&thread, NULL, step1, context);

    //  Wait for signal
    char *string = s_recv (receiver);
    free (string);

    //  Signal downstream to step 3
    void *sender = zmq_socket (context, ZMQ_PAIR);
    zmq_connect (sender, "inproc://step3");
    s_send (sender, "");

    return NULL;
}

int main () {
    void *context = zmq_init (1);

    //  Bind to inproc: endpoint, then start upstream thread
    void *receiver = zmq_socket (context, ZMQ_PAIR);
    zmq_bind (receiver, "inproc://step3");
    pthread_t thread;
    pthread_create (&thread, NULL, step2, context);

    //  Wait for signal
    char *string = s_recv (receiver);
    free (string);

    printf ("Test successful!\n");
    zmq_close (receiver);
    zmq_term (context);
    return 0;
}
