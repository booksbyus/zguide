//
//  Task worker in C - design 2
//  Adds pub-sub flow to receive and respond to kill signal
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
    void *control;          //  Socket for control input

    //  Prepare our context and sockets
    context = zmq_init (1);

    input = zmq_socket (context, ZMQ_PULL);
    zmq_connect (input, "tcp://localhost:5557");

    output = zmq_socket (context, ZMQ_PUSH);
    zmq_connect (output, "tcp://localhost:5558");

    control = zmq_socket (context, ZMQ_SUB);
    zmq_connect (control, "tcp://localhost:5559");
    zmq_setsockopt (control, ZMQ_SUBSCRIBE, "", 0);

    //  Process messages from input and control sockets
    zmq_pollitem_t items [2] = {
        { input,   0, ZMQ_POLLIN, 0 },
        { control, 0, ZMQ_POLLIN, 0 }
    };
    //  Process messages from both sockets
    while (1) {
        zmq_msg_t message;
        zmq_poll (&items [0], 2, -1);
        if (items [0].revents & ZMQ_POLLIN) {
            zmq_msg_init (&message);
            zmq_recv (input, &message, 0);
            
            //  Process task
            int workload;           //  Workload in msecs
            struct timespec t;
            sscanf ((char *) zmq_msg_data (&message), "%d", &workload);
            t.tv_sec = 0;
            t.tv_nsec = workload * 1000000;

            //  Do the work
            nanosleep (&t, NULL);

            //  Send results to sink
            zmq_msg_init (&message);
            zmq_send (output, &message, 0);

            //  Simple progress indicator for the viewer
            printf (".");
            fflush (stdout);
            
            zmq_msg_close (&message);
        }
        //  Any waiting control command acts as 'KILL'
        if (items [1].revents & ZMQ_POLLIN)
            break;                      //  Exit loop
    }
    //  Finished
    return 0;
}
