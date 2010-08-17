//
//  Simple request-reply broker
//
#include <zmq.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>

int main (int argc, char *argv[])
{
    void *context;          //  ØMQ context for our process
    void *frontend;         //  Socket facing clients
    void *backend;          //  Socket facing services
    int64_t more;           //  Multipart detection
    size_t more_size;

    //  Prepare our context and sockets
    context = zmq_init (1);
    frontend = zmq_socket (context, ZMQ_XREP);
    backend  = zmq_socket (context, ZMQ_XREQ);
    zmq_bind (frontend, "tcp://*:5559");
    zmq_bind (backend,  "tcp://*:5560");

    //  Initialize poll set
    zmq_pollitem_t items [2] = {
        { frontend, 0, ZMQ_POLLIN, 0 },
        { backend,  0, ZMQ_POLLIN, 0 }
    };
    //  Switch messages between sockets
    while (1) {
        zmq_msg_t message;
        zmq_poll (&items [0], 2, -1);
        if (items [0].revents & ZMQ_POLLIN) {
            while (1) {
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
        if (items [1].revents & ZMQ_POLLIN) {
            while (1) {
                //  Process all parts of the message
                zmq_msg_init (&message);
                zmq_recv (backend, &message, 0);
                more_size = sizeof (more);
                zmq_getsockopt (backend, ZMQ_RCVMORE, &more, &more_size);
                zmq_send (frontend, &message, more? ZMQ_SNDMORE: 0);
                zmq_msg_close (&message);
                if (!more)
                    break;      //  Last message part
            }
        }
    }
    return 0;
}
