//
//  Simple message queuing broker
//  Same as request-reply broker but using QUEUE device
//
#include "zhelpers.h"

int main (void) 
{
    void *context = zmq_ctx_new ();

    //  Socket facing clients
    void *frontend = zmq_socket (context, ZMQ_ROUTER);
    zmq_bind (frontend, "tcp://*:5559");

    //  Socket facing services
    void *backend = zmq_socket (context, ZMQ_DEALER);
    zmq_bind (backend, "tcp://*:5560");

    //  Start built-in device
    zmq_device (ZMQ_QUEUE, frontend, backend);

    //  We never get here...
    zmq_close (frontend);
    zmq_close (backend);
    zmq_ctx_destroy (context);
    return 0;
}
