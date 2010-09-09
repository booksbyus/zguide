//
//  Custom routing Router to Papa (XREP to REP)
//
#include "zhelpers.h"


//  We will do this all in one thread to emphasize the sequence
//  of events...
int main () {
    void *context = zmq_init (1);

    void *client = zmq_socket (context, ZMQ_XREP);
    zmq_bind (client, "ipc://routing");

    void *worker = zmq_socket (context, ZMQ_REP);
    zmq_setsockopt (worker, ZMQ_IDENTITY, "A", 1);
    zmq_connect (worker, "ipc://routing");

    //  Wait for sockets to stabilize
    sleep (1);

    //  Send papa address, address stack, empty part, and request
    s_sendmore (client, "A");
    s_sendmore (client, "address 3");
    s_sendmore (client, "address 2");
    s_sendmore (client, "address 1");
    s_sendmore (client, "");
    s_send     (client, "This is the workload");

    //  Worker should get just the workload
    s_dump (worker);

    //  We don't play with envelopes in the worker
    s_send (worker, "This is the reply");

    //  Now dump what we got off the XREP socket...
    s_dump (client);

    zmq_term (context);
    return 0;
}
