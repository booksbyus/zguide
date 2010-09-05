//
//  Multithreaded relay in C
//
#include "zhelpers.h"

//  Step 1 pushes one message to step 2

void *step1 (void *context) {
    void *sender = zmq_socket (context, ZMQ_PAIR);
    zmq_connect (sender, "inproc://step2");
    s_send (sender, "");
    return (NULL);
}

//  Step 2 relays the signal to step 3

void *step2 (void *context) {
    void *receiver = zmq_socket (context, ZMQ_PAIR);
    zmq_bind (receiver, "inproc://step2");
    char *string = s_recv (receiver);
    free (string);

    void *sender = zmq_socket (context, ZMQ_PAIR);
    zmq_connect (sender, "inproc://step3");
    s_send (sender, "");
    return (NULL);
}

//  Main program starts steps 1 and 2 and acts as step 3

int main () {
    void *context = zmq_init (1);
    void *receiver = zmq_socket (context, ZMQ_PAIR);
    zmq_bind (receiver, "inproc://step3");

    pthread_t thread;
    pthread_create (&thread, NULL, step2, context);
    //  Workaround for 0MQ issue 62
    sleep (1);
    pthread_create (&thread, NULL, step1, context);

    char *string = s_recv (receiver);
    free (string);

    printf ("Test successful!\n");
    return 0;
}
