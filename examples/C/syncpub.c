//
//  Synchronized publisher in C
//
#include <zmq.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

//  We wait for 10 subscribers
#define SUBSCRIBERS_EXPECTED  10

int main () {
    void *context;          //  ØMQ context for our process
    context = zmq_init (1);

    void *publisher;        //  Socket to talk to clients
    publisher = zmq_socket (context, ZMQ_PUB);
    zmq_bind (publisher, "tcp://*:5561");

    void *syncservice;         //  Socket to receive signals
    syncservice = zmq_socket (context, ZMQ_REP);
    zmq_bind (syncservice, "tcp://*:5562");

    //  Get synchronization from subscribers
    int subscribers = 0;
    while (subscribers < SUBSCRIBERS_EXPECTED) {
        //  - wait for synchronization request
        zmq_msg_t request;
        zmq_msg_init (&request);
        zmq_recv (syncservice, &request, 0);
        zmq_msg_close (&request);

        //  - send synchronization reply
        zmq_msg_t reply;
        zmq_msg_init (&reply);
        zmq_send (syncservice, &reply, 0);
        zmq_msg_close (&reply);

        subscribers++;
    }
    //  Now broadcast exactly 1M updates followed by END
    int update_nbr;
    for (update_nbr = 0; update_nbr < 1000000; update_nbr++) {
        zmq_msg_t update;
        zmq_msg_init_data (&update, "Rhubarb", 8, NULL, NULL);
        zmq_send (publisher, &update, 0);
        zmq_msg_close (&update);
    }
    zmq_msg_t end;
    zmq_msg_init_data (&end, "END", 4, NULL, NULL);
    zmq_send (publisher, &end, 0);
    zmq_msg_close (&end);

    sleep (1);              //  Give 0MQ time to flush output
    return 0;
}
