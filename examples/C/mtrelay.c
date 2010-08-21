//
//  Multithreaded relay in C
//
#include <zmq.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

//  Step 1 pushes one message to step 2

void *step1 (void *context) {
    void *sender;
    sender = zmq_socket (context, ZMQ_PAIR);
    zmq_connect (sender, "inproc://step2");

    zmq_msg_t message;
    zmq_msg_init (&message);
    zmq_send (sender, &message, 0);
    zmq_msg_close (&message);

    return (NULL);
}

//  Step 2 relays the signal to step 3

void *step2 (void *context) {
    void *receiver;
    receiver = zmq_socket (context, ZMQ_PAIR);
    zmq_bind (receiver, "inproc://step2");

    void *sender;
    sender = zmq_socket (context, ZMQ_PAIR);
    zmq_connect (sender, "inproc://step3");

    zmq_msg_t message;
    zmq_msg_init (&message);
    zmq_recv (receiver, &message, 0);
    zmq_msg_close (&message);

    zmq_msg_init (&message);
    zmq_send (sender, &message, 0);
    zmq_msg_close (&message);

    return (NULL);
}

//  Main program starts steps 1 and 2 and acts as step 3

int main () {
    void *context;          //  ØMQ context for our process
    context = zmq_init (1);

    void *receiver;
    receiver = zmq_socket (context, ZMQ_PAIR);
    zmq_bind (receiver, "inproc://step3");

    pthread_t thread;
    pthread_create (&thread, NULL, step2, context);
    pthread_create (&thread, NULL, step1, context);

    zmq_msg_t message;
    zmq_msg_init (&message);
    zmq_recv (receiver, &message, 0);
    zmq_msg_close (&message);
    printf ("Test successful!\n");

    return 0;
}
