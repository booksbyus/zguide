//
//  Cross-connected XREP sockets addressing each other
//
#include "zhelpers.h"

int main () {
    void *context = zmq_init (1);

    void *server = zmq_socket (context, ZMQ_XREP);
    zmq_setsockopt (server, ZMQ_IDENTITY, "SERVER", 6);
    zmq_bind (server, "ipc://rtrouter.ipc");

    void *client = zmq_socket (context, ZMQ_XREP);
    zmq_setsockopt (client, ZMQ_IDENTITY, "CLIENT", 6);
    zmq_connect (client, "ipc://rtrouter.ipc");

    //  Give client time to connect...
    sleep (1);

    s_sendmore (client, "SERVER");
    s_sendmore (client, "");
    s_send (client, "send to client");
    s_dump (server);

    s_sendmore (server, "CLIENT");
    s_sendmore (server, "");
    s_send (server, "send to server");
    s_dump (client);

    zmq_term (context);
    return 0;
}
