//
//  Synchronized subscriber in C
//
#include <zmq.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

int main (int argc, char *argv[])
{
    void *context;          //  ØMQ context for our process
    context = zmq_init (1);

    //  First, connect our subscriber socket
    void *subscriber;
    subscriber = zmq_socket (context, ZMQ_SUB);
    zmq_connect (subscriber, "tcp://localhost:5561");
    zmq_setsockopt (subscriber, ZMQ_SUBSCRIBE, "", 0);

    //  Second, synchronize with publisher
    void *syncclient;
    syncclient = zmq_socket (context, ZMQ_REQ);
    zmq_connect (syncclient, "tcp://localhost:5562");

    //  - send a synchronization request
    zmq_msg_t request;
    zmq_msg_init (&request);
    zmq_send (syncclient, &request, 0);
    zmq_msg_close (&request);

    //  - wait for synchronization reply
    zmq_msg_t reply;
    zmq_msg_init (&reply);
    zmq_recv (syncclient, &reply, 0);
    zmq_msg_close (&reply);

    //  Third, get our updates and report how many we got
    int update_nbr = 0;
    while (1) {
        zmq_msg_t update;
        zmq_msg_init (&update);
        zmq_recv (subscriber, &update, 0);
        if (zmq_msg_size (&update) == 4
        && memcmp (zmq_msg_data (&update), "END", 4) == 0)
            break;
        zmq_msg_close (&update);
        update_nbr++;
    }
    printf ("Received %d updates\n", update_nbr);

    return 0;
}
