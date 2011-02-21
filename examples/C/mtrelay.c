//
//  Multithreaded relay
//
//  Changes for 2.1:
//  - added version assertion
//  - pass context & thread to child threads
//  - create socket pair for inproc communications
//  - close sockets in each child thread
//
#include "zhelpers.h"

//  This is the structure we pass to child threads
typedef struct {
    void *context;
    void *socket;
} thread_args_t;

//  Each thread receives a threads_args_t structure that it uses
//  It frees its arguments when done with them.
static void *
step1 (void *args) {
    thread_args_t *self = (thread_args_t *) args;

    //  Signal downstream to step 2
    s_send (self->socket, "");
    zmq_close (self->socket);
    free (self);
    return NULL;
}

static void *
step2 (void *args) {
    thread_args_t *self = (thread_args_t *) args;

    //  Create socket pair, then pass one socket to upstream thread
    thread_args_t *child = malloc (sizeof (thread_args_t));
    child->context = self->context;
    child->socket = zmq_socket (self->context, ZMQ_PAIR);

    void *receiver = zmq_socket (self->context, ZMQ_PAIR);
    zmq_bind (receiver, "inproc://step2");
    zmq_connect (child->socket, "inproc://step2");
    pthread_t thread;
    pthread_create (&thread, NULL, step1, child);

    //  Wait for signal
    char *string = s_recv (receiver);
    free (string);
    zmq_close (receiver);

    //  Signal downstream to step 3
    s_send (self->socket, "");
    zmq_close (self->socket);
    free (self);
    return NULL;
}

int main () {
    s_version_assert (2, 1);
    void *context = zmq_init (1);

    //  Create socket pair, then pass one socket to upstream thread
    thread_args_t *child = malloc (sizeof (thread_args_t));
    child->context = context;
    child->socket = zmq_socket (context, ZMQ_PAIR);

    void *receiver = zmq_socket (context, ZMQ_PAIR);
    zmq_bind (receiver, "inproc://step3");
    zmq_connect (child->socket, "inproc://step3");
    pthread_t thread;
    pthread_create (&thread, NULL, step2, child);

    //  Wait for signal
    char *string = s_recv (receiver);
    free (string);

    printf ("Test successful!\n");
    zmq_close (receiver);
    zmq_term (context);
    return 0;
}
