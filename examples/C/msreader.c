//
//  Reading from multiple sockets in C
//  This version uses a simple recv loop
//
#include <zmq.h>
#include <time.h>

int main (int argc, char *argv[])
{
    void *context;          //  ØMQ context for our process
    void *receiver;         //  PULL receiver
    void *subscriber;       //  PUSH susbcriber

    //  Prepare our context and sockets
    context = zmq_init (1);

    //  Connect to task ventilator
    receiver = zmq_socket (context, ZMQ_PULL);
    zmq_connect (receiver, "tcp://localhost:5557");

    //  Connect to weather server
    subscriber = zmq_socket (context, ZMQ_SUB);
    zmq_connect (subscriber, "tcp://localhost:5556");
    zmq_setsockopt (subscriber, ZMQ_SUBSCRIBE, "10001 ", 6);

    //  Process messages from both sockets
    //  We prioritize traffic from the task ventilator
    while (1) {
        int rc;
        struct timespec t;
        zmq_msg_t message;

        //  Process any waiting tasks
        for (rc = 0; !rc; ) {
            zmq_msg_init (&message);
            if ((rc = zmq_recv (receiver, &message, ZMQ_NOBLOCK)) == 0) {
                //  process task
            }
            zmq_msg_close (&message);
        }
        //  Process any waiting weather updates
        for (rc = 0; !rc; ) {
            zmq_msg_init (&message);
            if ((rc = zmq_recv (subscriber, &message, ZMQ_NOBLOCK)) == 0) {
                //  process weather update
            }
            zmq_msg_close (&message);
        }
        //  No activity, so sleep for 1 msec
        t.tv_sec = 0;
        t.tv_nsec = 1000000;
        nanosleep (&t, NULL);
    }
    return 0;
}
