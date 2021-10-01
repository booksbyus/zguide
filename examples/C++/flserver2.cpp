//  Freelance server - Model 2
//  Does some work, replies OK, with message sequencing

#include <iostream>
#include <zmqpp/zmqpp.hpp>

int main(int argc, char *argv[]) {
    if (argc < 2) {
        std::cout << "I: syntax: " << argv[0] << " <endpoint>" << std::endl;
        return 0;
    }
    zmqpp::context context;
    zmqpp::socket server(context, zmqpp::socket_type::reply);
    server.bind(argv[1]);

    std::cout << "I: echo service is ready at " << argv[1] << std::endl;
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
        //  Fail nastily if run against wrong client
        assert(request.parts() == 2);

        uint identity;
        request.get(identity, 0);
        // std::cout << "Received sequence: " << identity << std::endl;

        zmqpp::message reply;
        reply.push_back(identity);
        reply.push_back("OK");

        server.send(reply);
    }
    return 0;
}
