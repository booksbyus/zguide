//
//  Task worker
//  Connects PULL socket to tcp://localhost:5557
//  Collects workloads from ventilator via that socket
//  Connects PUSH socket to tcp://localhost:5558
//  Sends results to sink via that socket
//
#include "zhelpers.h"

int main (int argc, char *argv[])
{
    void *context = zmq_init (1);

    //  Socket to receive messages on
    void *receiver = zmq_socket (context, ZMQ_PULL);
    zmq_connect (receiver, "tcp://localhost:5557");

    //  Socket to send messages to
    void *sender = zmq_socket (context, ZMQ_PUSH);
    zmq_connect (sender, "tcp://localhost:5558");

    //  Process tasks forever
    while (1) {
        char *string = s_recv (receiver);
        struct timespec t;
        t.tv_sec = 0;
        t.tv_nsec = atoi (string) * 1000000;
        //  Simple progress indicator for the viewer
        fflush (stdout);
        printf ("%s.", string);
        free (string);

        //  Do the work
        nanosleep (&t, NULL);

        //  Send results to sink
        s_send (sender, "");
    }
    return 0;
}
