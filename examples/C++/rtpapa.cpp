//
//  Custom routing Router to Papa (XREP to REP)
//
// Olivier Chamoux <olivier.chamoux@fr.thalesgroup.com>

#include "zhelpers.hpp"

//  We will do this all in one thread to emphasize the sequence
//  of events...
int main () {
    zmq::context_t context(1);

    zmq::socket_t client (context, ZMQ_XREP);
   client.bind("ipc://routing");

    zmq::socket_t worker (context, ZMQ_REP);
    worker.setsockopt(ZMQ_IDENTITY, "A", 1);
    worker.connect("ipc://routing");

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

    return 0;
}
