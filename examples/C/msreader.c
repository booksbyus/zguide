//
//  Reading from multiple sockets
//  This version uses a simple recv loop
//
#include "zhelpers.h"

int main (void) 
{
    //  Prepare our context and sockets
    void *context = zmq_init (1);

    //  Connect to task ventilator
    void *receiver = zmq_socket (context, ZMQ_PULL);
    zmq_connect (receiver, "tcp://localhost:5557");

    //  Connect to weather server
    void *subscriber = zmq_socket (context, ZMQ_SUB);
    zmq_connect (subscriber, "tcp://localhost:5556");
    zmq_setsockopt (subscriber, ZMQ_SUBSCRIBE, "10001 ", 6);

    //  Process messages from both sockets
    //  We prioritize traffic from the task ventilator
    while (1) {
        //  Process any waiting tasks
        int rc;
        for (rc = 0; !rc; ) {
            zmq_msg_t task;
            zmq_msg_init (&task);
            if ((rc = zmq_recv (receiver, &task, ZMQ_NOBLOCK)) == 0) {
                //  process task
            }
            zmq_msg_close (&task);
        }
        //  Process any waiting weather updates
        for (rc = 0; !rc; ) {
            zmq_msg_t update;
            zmq_msg_init (&update);
            if ((rc = zmq_recv (subscriber, &update, ZMQ_NOBLOCK)) == 0) {
                //  process weather update
            }
            zmq_msg_close (&update);
        }
        //  No activity, so sleep for 1 msec
        s_sleep (1);
    }
    //  We never get here but clean up anyhow
    zmq_close (receiver);
    zmq_close (subscriber);
    zmq_term (context);
    return 0;
}
