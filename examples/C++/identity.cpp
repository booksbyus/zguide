//
//  Demonstrate identities as used by the request-reply pattern.  Run this
//  program by itself.
//

#include <zmq.hpp>
#include "zhelpers.hpp"

int main () {
    zmq::context_t context(1);

    zmq::socket_t sink(context, ZMQ_ROUTER);
    sink.bind( "inproc://example");

    //  First allow 0MQ to set the identity
    zmq::socket_t anonymous(context, ZMQ_REQ);
    anonymous.connect( "inproc://example");

    s_send (anonymous, std::string("ROUTER uses a generated 5 byte identity"));
    s_dump (sink);

    //  Then set the identity ourselves
    zmq::socket_t identified (context, ZMQ_REQ);
    identified.set( zmq::sockopt::routing_id, "PEER2");
    identified.connect( "inproc://example");

    s_send (identified, std::string("ROUTER socket uses REQ's socket identity"));
    s_dump (sink);

    return 0;
}
