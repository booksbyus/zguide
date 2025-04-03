//
//  Hello World server in C++
//  Binds REP socket to tcp://*:5555
//  Expects "Hello" from client, replies with "World"
//
#include <zmq.hpp>
#include <string>
#include <iostream>
#ifndef _WIN32
#include <unistd.h>
#else
#include <windows.h>

#define sleep(n)	Sleep(n)
#endif

int main () {
    //  Prepare our context and socket
    static const int kNumberOfThreads = 2;
    zmq::context_t context (kNumberOfThreads);
    zmq::socket_t socket (context, zmq::socket_type::rep);
    socket.bind ("tcp://*:5555");

    while (true) {
        zmq::message_t request;

        //  Wait for next request from client
        auto result = socket.recv (request, zmq::recv_flags::none);
        assert(result.value_or(0) != 0); // Check if bytes received is non-zero
        std::cout << "Received Hello" << std::endl;

        //  Pretend to do some 'work'
        sleep(1);

        //  Send reply back to client
        constexpr std::string_view kReplyString = "World";
        zmq::message_t reply (kReplyString.length());
        memcpy (reply.data (), kReplyString.data(), kReplyString.length());
        socket.send (reply, zmq::send_flags::none);
    }
    return 0;
}
