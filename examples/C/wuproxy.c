//
//  Weather proxy device
//
#include <zmq.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>

int main (int argc, char *argv[])
{
    void *context;          //  ØMQ context for our process
    void *frontend;         //  Socket facing outside
    void *backend;          //  Socket facing frontend

    //  This is where the weather server sits
    char *frontend_endpoint = "tcp://192.168.55.210:5556";

    //  This is our public endpoint for subscribers
    char *backend_endpoint = "tcp://10.1.1.0:8100";

    //  Prepare our context and sockets
    context  = zmq_init (1);
    frontend = zmq_socket (context, ZMQ_SUB);
    backend  = zmq_socket (context, ZMQ_PUB);
    zmq_connect (frontend, frontend_endpoint);
    zmq_bind    (backend,  backend_endpoint);

    //  Subscribe on everything
    zmq_setsockopt (frontend, ZMQ_SUBSCRIBE, "", 0);

    //  Shunt messages out to our own subscribers
    while (1) {
        while (1) {
            zmq_msg_t message;
            int64_t more;
            size_t more_size;

            //  Process all parts of the message
            zmq_msg_init (&message);
            zmq_recv (frontend, &message, 0);
            more_size = sizeof (more);
            zmq_getsockopt (frontend, ZMQ_RCVMORE, &more, &more_size);
            zmq_send (backend, &message, more? ZMQ_SNDMORE: 0);
            zmq_msg_close (&message);
            if (!more)
                break;      //  Last message part
        }
    }
    return 0;
}
