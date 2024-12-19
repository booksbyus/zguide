// Binary Star server, using bstar reactor

#include "bstar.hpp"

// Echo service
void s_echo(zmqpp::loop *loop, zmqpp::socket_t *socket, void *arg) {
    zmqpp::message_t msg;
    socket->receive(msg);
    socket->send(msg);
}

int main(int argc, char *argv []) {
    //  Arguments can be either of:
    //      -p  primary server, at tcp://localhost:5001
    //      -b  backup server, at tcp://localhost:5002
    
    bstar_t *bstar = nullptr;
    if (argc == 2 && strcmp(argv[1], "-p") == 0) {
        std::cout << "I: Primary active, waiting for backup (passive)" << std::endl;
        bstar = new bstar_t(true, "tcp://*:5003", "tcp://localhost:5004");
        bstar->register_voter("tcp://*:5001", zmqpp::socket_type::router, s_echo, NULL);
    } else if (argc == 2 && strcmp(argv[1], "-b") == 0) {
        std::cout << "I: Backup passive, waiting for primary (active)" << std::endl;
        bstar = new bstar_t(false, "tcp://*:5004", "tcp://localhost:5003");
        bstar->register_voter("tcp://*:5002", zmqpp::socket_type::router, s_echo, NULL);
    } else {
        std::cout << "Usage: bstarsrv2 { -p | -b }" << std::endl;
        return 0;
    }

    try {
        std::cout << "starting bstar...\n";
        bstar->start();
    } catch (const std::exception &e) {
        std::cerr << "Exception: " << e.what() << std::endl;
    }
    delete bstar;
    return 0;
}