//  Freelance server - Model 1
//  Trivial echo service

#include <iostream>
#include <zmq.hpp>

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cout << "I: syntax: " << argv[0] << " <endpoint>" << std::endl;
        return 0;
    }
    zmq::context_t context{1};
    zmq::socket_t server(context, zmq::socket_type::rep);
    server.bind(argv[1]);

    std::cout << "I: echo service is ready at " << argv[1] << std::endl;

    while (true) {
        zmq::message_t message;
        try {
            server.recv(message, zmq::recv_flags::none);
        } catch (zmq::error_t& e) {
            if (e.num() == EINTR)
                std::cout << "W: interrupted" << std::endl;
            else
                std::cout << "E: error, errnum = " << e.num() << ", what = " << e.what()
                          << std::endl;
            break;  // Interrupted
        }
        server.send(message, zmq::send_flags::none);
    }
    return 0;
}
