//
//  Multithreaded Hello World server in C
//
#include <zmq.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

void *worker_routine (void *context) {
    //  Socket to talk to dispatcher
    void *receiver = zmq_socket (context, ZMQ_REP);
    zmq_connect (receiver, "inproc://workers");

    while (1) {
        //  Wait for next request from client
        zmq_msg_t request;
        zmq_msg_init (&request);
        zmq_recv (receiver, &request, 0);
        printf ("Received request: [%s]\n",
            (char *) zmq_msg_data (&request));
        zmq_msg_close (&request);

        //  Do some 'work'
        sleep (1);

        //  Send reply back to client
        zmq_msg_t reply;
        zmq_msg_init_size (&reply, 6);
        memcpy ((void *) zmq_msg_data (&reply), "World", 6);
        zmq_send (receiver, &reply, 0);
        zmq_msg_close (&reply);
    }
    return (NULL);
}

int main () {
    //  Prepare our context and sockets
    void *context = zmq_init (1);

    //  Socket to talk to clients
    void *clients = zmq_socket (context, ZMQ_XREP);
    zmq_bind (clients, "tcp://*:5555");

    //  Socket to talk to workers
    void *workers = zmq_socket (context, ZMQ_XREQ);
    zmq_bind (workers, "inproc://workers");

    //  Launch pool of worker threads
    int thread_nbr;
    for (thread_nbr = 0; thread_nbr != 5; thread_nbr++) {
        pthread_t worker;
        pthread_create (&worker, NULL, worker_routine, context);
    }
    //  Connect work threads to client threads via a queue
    zmq_device (ZMQ_QUEUE, clients, workers);
    return 0;
}
