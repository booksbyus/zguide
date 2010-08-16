//
//  Weather proxy/bridge device
//
#include <zmq.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

int main (int argc, char *argv[])
{
    void *context;          //  ØMQ context for our process
    void *frontend;          //  Socket facing outside
    void *backend;         //  Socket facing frontend

    //  This is where the weather update server sits
    char *frontend_endpoint = "tcp://192.168.55.210:5556";

    //  This is our public IP address and port
    char *backend_endpoint = "tcp://10.1.1.0:8100";

    //  Prepare our context and sockets
    context  = zmq_init (1);
    frontend  = zmq_socket (context, ZMQ_SUB);
    backend = zmq_socket (context, ZMQ_PUB);

    zmq_connect (frontend,  frontend_endpoint);
    zmq_bind    (backend, backend_endpoint);
    
    //  Subscribe on everything
    zmq_setsockopt (frontend, ZMQ_SUBSCRIBE, "", 0);

    //  Start the forwarder device
    zmq_device (ZMQ_FORWARDER, frontend, backend);
    return 0;
}
