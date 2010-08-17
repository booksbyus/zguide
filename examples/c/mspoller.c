//
//  Reading from multiple sockets in C
//  This version uses zmq_poll()
//
#include <zmq.h>
#include <time.h>

int main (int argc, char *argv[])
{
    void *context;          //  ØMQ context for our process
    void *s1, *s2;

    //  Prepare our context and sockets
    context = zmq_init (1);

    //  Connect to task ventilator
    s1 = zmq_socket (context, ZMQ_PULL);
    zmq_connect (s1, "tcp://localhost:5557");

    //  Connect to weather server
    s2 = zmq_socket (context, ZMQ_SUB);
    zmq_connect (s2, "tcp://localhost:5556");
    zmq_setsockopt (s2, ZMQ_SUBSCRIBE, "10001 ", 6);

    //  Initialize poll set
    zmq_pollitem_t items [2] = {
        { s1, 0, ZMQ_POLLIN, 0 },
        { s2, 0, ZMQ_POLLIN, 0 }
    };
    //  Process messages from both sockets
    while (1) {
        zmq_msg_t message;
        zmq_poll (&items [0], 2, -1);
        if (items [0].revents & ZMQ_POLLIN) {
            zmq_msg_init (&message);
            zmq_recv (s1, &message, 0);
            //  Process task
            zmq_msg_close (&message);
        }
        if (items [1].revents & ZMQ_POLLIN) {
            zmq_msg_init (&message);
            zmq_recv (s2, &message, 0);
            //  Process weather update
            zmq_msg_close (&message);
        }
    }
    return 0;
}
