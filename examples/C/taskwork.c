//
//  Task worker in C
//  Connects PULL socket to tcp://localhost:5557
//  Collects workloads from ventilator via that socket
//  Connects PUSH socket to tcp://localhost:5558
//  Sends results to sink via that socket
//
#include <zmq.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <time.h>

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
        zmq_msg_t message;
        int workload;           //  Workload in msecs
        struct timespec t;

        zmq_msg_init (&message);
        zmq_recv (receiver, &message, 0);
        sscanf ((char *) zmq_msg_data (&message), "%d", &workload);
        t.tv_sec = 0;
        t.tv_nsec = workload * 1000000;
        zmq_msg_close (&message);

        //  Do the work
        nanosleep (&t, NULL);

        //  Send results to sink
        zmq_msg_init (&message);
        zmq_send (sender, &message, 0);

        //  Simple progress indicator for the viewer
        printf (".");
        fflush (stdout);
    }
    return 0;
}
