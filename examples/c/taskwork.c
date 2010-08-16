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
    void *context;          //  ØMQ context for our process
    void *input, *output;   //  Sockets for input and output

    //  Prepare our context and sockets
    context = zmq_init (1);

    input = zmq_socket (context, ZMQ_PULL);
    zmq_connect (input, "tcp://localhost:5557");

    output = zmq_socket (context, ZMQ_PUSH);
    zmq_connect (output, "tcp://localhost:5558");

    //  Process tasks forever
    while (1) {
        zmq_msg_t message;
        int workload;           //  Workload in msecs
        struct timespec t;

        zmq_msg_init (&message);
        zmq_recv (input, &message, 0);
        sscanf ((char *) zmq_msg_data (&message), "%d", &workload);
        t.tv_sec = 0;
        t.tv_nsec = workload * 1000000;
        zmq_msg_close (&message);

        //  Do the work
        nanosleep (&t, NULL);

        //  Send results to sink
        zmq_msg_init (&message);
        zmq_send (output, &message, 0);

        //  Simple progress indicator for the viewer
        printf (".");
        fflush (stdout);
    }
    return 0;
}
