//
//  Demonstrate identities as used by the request-reply pattern.  Run this
//  program by itself.  Note that the utility functions s_ are provided by
//  zhelpers.h.  It gets boring for everyone to keep repeating this code.
//
#include "zhelpers.h"

int main () {
    void *context = zmq_init (1);

    //  First allow 0MQ to set the identity
    void *sink = zmq_socket (context, ZMQ_XREP);
    zmq_bind (sink, "inproc://example");

    void *anonymous = zmq_socket (context, ZMQ_REQ);
    zmq_connect (anonymous, "inproc://example");
    s_send (anonymous, "XREP uses a generated UUID");
    s_dump (sink);

    void *identified = zmq_socket (context, ZMQ_REQ);
    zmq_setsockopt (identified, ZMQ_IDENTITY, "Hello", 5);
    zmq_connect (identified, "inproc://example");
    s_send (identified, "XREP socket uses REQ's socket identity");
    s_dump (sink);

    return 0;
}
