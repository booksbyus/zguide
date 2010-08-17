//
//  Multithreaded Hello World server in C
//
#include <zmq.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

void *worker_routine (void *context) {
    void *socket;           //  Socket to talk to dispatcher

    socket = zmq_socket (context, ZMQ_REP);
    zmq_connect (socket, "inproc://workers");

    while (1) {
        zmq_msg_t request, reply;

        //  Wait for next request from client
        zmq_msg_init (&request);
        zmq_recv (socket, &request, 0);
        printf ("Received request: [%s]\n",
            (char *) zmq_msg_data (&request));
        zmq_msg_close (&request);

        //  Do some 'work'
        sleep (1);

        //  Send reply back to client
        zmq_msg_init_size (&reply, 6);
        memcpy ((void *) zmq_msg_data (&reply), "World", 6);
        zmq_send (socket, &reply, 0);
        zmq_msg_close (&reply);
    }
    return (NULL);
}

int main () {
    void *context;          //  ØMQ context for our process
    void *clients;          //  Socket to talk to clients
    void *workers;          //  Socket to talk to workers
    int thread_nbr;

    //  Prepare our context and sockets
    context = zmq_init (1);
    clients = zmq_socket (context, ZMQ_XREP);
    zmq_bind (clients, "tcp://*:5555");
    workers = zmq_socket (context, ZMQ_XREQ);
    zmq_bind (workers, "inproc://workers");

    //  Launch pool of worker threads
    for (thread_nbr = 0; thread_nbr != 5; thread_nbr++) {
        pthread_t worker;
        pthread_create (&worker, NULL, worker_routine, context);
    }
    //  Connect work threads to client threads via a queue
    zmq_device (ZMQ_QUEUE, clients, workers);
    return 0;
}
