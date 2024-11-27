//
//   Request-reply service in C++
//   Connects REP socket to tcp://localhost:5560
//   Expects "Hello" from client, replies with "World"
//


#include <zmq.hpp>
#include <chrono>
#include <thread>

int main(int argc, char* argv[])
{
    zmq::context_t context{1};

    zmq::socket_t responder{context, zmq::socket_type::rep};
    responder.connect("tcp://localhost:5560");

    while (true) {
        //  Wait for next request from client
        zmq::message_t request_msg;
        auto recv_result = responder.recv(request_msg, zmq::recv_flags::none);
        std::string string = request_msg.to_string();

        std::cout << "Received request: " << string << std::endl;

        // Do some 'work'
        std::this_thread::sleep_for(std::chrono::seconds(1));

        //  Send reply back to client
        zmq::message_t reply_msg{std::string{"World"}};
        responder.send(reply_msg, zmq::send_flags::none);

    }
}
