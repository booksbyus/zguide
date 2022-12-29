//
//  Pubsub envelope publisher
//  Note that the zhelpers.h file also provides s_sendmore
//

#include "zhelpers.hpp"

int main () {
    //  Prepare our context and publisher
    zmq::context_t context(1);
    zmq::socket_t publisher(context, ZMQ_PUB);
    publisher.bind("tcp://*:5563");

    while (1) {
        //  Write two messages, each with an envelope and content
        s_sendmore (publisher, std::string("A"));
        s_send (publisher, std::string("We don't want to see this"));
        s_sendmore (publisher, std::string("B"));
        s_send (publisher, std::string("We would like to see this"));
        sleep (1);
    }
    return 0;
}
