//
//  Multithreaded pipeline test in C
TODO
//
#include <zmq.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>


void *step1 (void *context) {
    void *output;
    output = zmq_socket (context, ZMQ_PUSH);
    assert (output);
    assert (!zmq_connect (output, "inproc://step2"));

    zmq_msg_t message;
    zmq_msg_init (&message);
    assert (!zmq_send (output, &message, 0));
    zmq_msg_close (&message);

    return (NULL);
}

void *step2 (void *context) {
    void *input;
    input = zmq_socket (context, ZMQ_PULL);
    assert (input);
    assert (!zmq_bind (input, "inproc://step2"));

    void *output;
    output = zmq_socket (context, ZMQ_PUSH);
    assert (output);
    assert (!zmq_connect (output, "inproc://step3"));

    zmq_msg_t message;
    zmq_msg_init (&message);
    assert (!zmq_recv (input, &message, 0));
    zmq_msg_close (&message);

    zmq_msg_init (&message);
    assert (!zmq_send (output, &message, 0));
    zmq_msg_close (&message);

    return (NULL);
}

int main () {
    void *context;          //  ØMQ context for our process
    context = zmq_init (1);

    void *input;
    input = zmq_socket (context, ZMQ_PULL);
    assert (input);
    assert (!zmq_bind (input, "inproc://step3"));

    pthread_t thread;
    pthread_create (&thread, NULL, step2, context);
    pthread_create (&thread, NULL, step1, context);

    zmq_msg_t message;
    zmq_msg_init (&message);
    assert (!zmq_recv (input, &message, 0));
    zmq_msg_close (&message);
    printf ("Test successful!\n");

    return 0;
}
