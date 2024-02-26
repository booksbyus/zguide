//
// Created by ninehs on 4/29/22.
//

//
// Broker peering simulation (part 1)
// Prototypes the state flow
//

#include "zhelpers.hpp"

#define ZMQ_POLL_MSEC 1

int main(int argc, char *argv[]) {
    // First argument is this broker's name
    // Other arguments are our peers' names

    if (argc < 2) {
        std::cout << "syntax: peering1 me {you} ..." << std::endl;
        return 0;
    }

    std::string self(argv[1]);
    std::cout << "I: preparing broker at " << self << " ..." << std::endl;
    srandom(static_cast<unsigned int>(time(nullptr)));

    zmq::context_t context(1);

    // Bind state backend to endpoint
    zmq::socket_t statebe(context, zmq::socket_type::pub);
    std::string bindURL = std::string("ipc://").append(self).append("-state.ipc");
    statebe.bind(bindURL);

    // Connect statefe to all peers
    zmq::socket_t statefe(context, zmq::socket_type::sub);
    statefe.set(zmq::sockopt::subscribe, "");
    for(int argn = 2 ; argn < argc ; ++argn) {
        std::string peer(argv[argn]);
        std::string peerURL = std::string("ipc://").append(peer).append("-state.ipc");
        statefe.connect(peerURL);
    }

    // The main loop sends out status messages to peers, and collects
    // status messages back from peers. The zmq_poll timeout defines
    // our own heartbeat
    while(true) {
        //
        zmq::pollitem_t items[] = {
                {statefe, 0, ZMQ_POLLIN, 0}
        };

        try {
            zmq::poll(items, 1, 1000 * ZMQ_POLL_MSEC);
        } catch(...) {
            break;
        }

        if (items[0].revents & ZMQ_POLLIN) {
            std::string peer_name(s_recv(statefe));
            std::string available(s_recv(statefe));
            std::cout << "\"" << self << "\" received subscribed message: \"" << peer_name << "\" has "
                      << available << " workers available" << std::endl;
        } else {
            s_sendmore(statebe, self);
            std::ostringstream intStream;
            intStream << within(10);
            s_send(statebe, intStream.str());
            std::cout << "\"" << self << "\" broadcast:  " << intStream.str() << " workers available." << std::endl;
        }
    }
    return 0;
}