//
//  Simple message queuing broker
//  Same as request-reply broker but using QUEUE device
//
#include <zmq.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

int main (int argc, char *argv[])
{
    void *context;          //  ØMQ context for our process
    void *frontend;         //  Socket facing clients
    void *backend;          //  Socket facing services

    //  Prepare our context and sockets
    context = zmq_init (1);
    frontend = zmq_socket (context, ZMQ_XREP);
    backend  = zmq_socket (context, ZMQ_XREQ);
    zmq_bind (frontend, "tcp://*:5559");
    zmq_bind (backend,  "tcp://*:5560");

    //  Start built-in device
    zmq_device (ZMQ_QUEUE, frontend, backend);
    return 0;
}
