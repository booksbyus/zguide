//  Freelance server - Model 3
//  Uses an ROUTER/ROUTER socket but just one thread

#include <iostream>
#include <zmqpp/zmqpp.hpp>

void dump_binary(const zmqpp::message &msg);

int main(int argc, char *argv[]) {
    int verbose = (argc > 1 && (std::string(argv[1]) == "-v"));
    if (verbose) std::cout << "verbose active" << std::endl;
    zmqpp::context context;

    //  Prepare server socket with predictable identity
    std::string bind_endpoint = "tcp://*:5555";
    std::string connect_endpoint = "tcp://localhost:5555";
    zmqpp::socket server(context, zmqpp::socket_type::router);
    server.set(zmqpp::socket_option::identity, connect_endpoint);
    server.bind(bind_endpoint);
    std::cout << "I: service is ready at " << bind_endpoint << std::endl;

    while (true) {
        zmqpp::message request;
        try {
            server.receive(request);
        } catch (zmqpp::zmq_internal_exception &e) {
            if (e.zmq_error() == EINTR)
                std::cout << "W: interrupted" << std::endl;
            else
                std::cout << "E: error, errnum = " << e.zmq_error() << ", what = " << e.what()
                          << std::endl;
            break;  // Interrupted
        }
        if (verbose) {
            std::cout << "Message received from client, all data will dump. " << std::endl;
            dump_binary(request);
        }

        //  Frame 0: identity of client
        //  Frame 1: PING, or client control frame
        //  Frame 2: request body
        std::string identity;
        std::string control;
        request >> identity >> control;

        zmqpp::message reply;
        if (control == "PING")
            reply.push_back("PONG");
        else {
            reply.push_back(control);
            reply.push_back("OK");
        }
        reply.push_front(identity);
        if (verbose) {
            std::cout << "Message reply to client dump." << std::endl;
            dump_binary(reply);
        }
        server.send(reply);
    }
    return 0;
}

void dump_binary(const zmqpp::message &msg) {
    std::cout << "Dump message ..." << std::endl;
    for (size_t part = 0; part < msg.parts(); ++part) {
        std::cout << "Part: " << part << std::endl;
        const unsigned char *bin = static_cast<const unsigned char *>(msg.raw_data(part));
        for (size_t i = 0; i < msg.size(part); ++i) {
            std::cout << std::hex << static_cast<uint16_t>(*(bin++)) << " ";
        }
        std::cout << std::endl;
    }
    std::cout << "Dump finish ..." << std::endl;
}
