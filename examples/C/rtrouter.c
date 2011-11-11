//
//  Cross-connected ROUTER sockets addressing each other
//
#include "zhelpers.h"

int main (void) 
{
    void *context = zmq_init (1);

    void *worker = zmq_socket (context, ZMQ_ROUTER);
    zmq_setsockopt (worker, ZMQ_IDENTITY, "WORKER", 6);
    zmq_bind (worker, "ipc://rtrouter.ipc");

    void *server = zmq_socket (context, ZMQ_ROUTER);
    zmq_setsockopt (server, ZMQ_IDENTITY, "SERVER", 6);
    zmq_connect (server, "ipc://rtrouter.ipc");

    //  Wait for the worker to connect so that when we send a message
    //  with routing envelope, it will actually match the worker...
    sleep (1);

    s_sendmore (server, "WORKER");
    s_sendmore (server, "");
    s_send     (server, "send to worker");
    s_dump     (worker);

    s_sendmore (worker, "SERVER");
    s_sendmore (worker, "");
    s_send     (worker, "send to server");
    s_dump     (server);

    zmq_close (worker);
    zmq_close (server);
    zmq_term (context);
    return 0;
}
