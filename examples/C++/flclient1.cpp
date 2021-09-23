//  Freelance client - Model 1
//  Uses REQ socket to query one or more services
#include <iostream>
#include <zmq.hpp>
#include <zmq_addon.hpp>

const int REQUEST_TIMEOUT = 1000;
const int MAX_RETRIES = 3;  //  Before we abandon

static std::unique_ptr<zmq::message_t> s_try_request(zmq::context_t &context,
                                                     const std::string &endpoint,
                                                     const zmq::const_buffer &request) {
    std::cout << "I: trying echo service at " << endpoint << std::endl;
    zmq::socket_t client(context, zmq::socket_type::req);

    // Set ZMQ_LINGER to REQUEST_TIMEOUT milliseconds, otherwise if we send a message to a server
    // that is not working properly or even not exist, we may never be able to exit the program
    client.setsockopt(ZMQ_LINGER, REQUEST_TIMEOUT);

    client.connect(endpoint);

    //  Send request, wait safely for reply
    zmq::message_t message(request.data(), request.size());
    client.send(message, zmq::send_flags::none);

    zmq_pollitem_t items[] = {{client, 0, ZMQ_POLLIN, 0}};
    zmq::poll(items, 1, REQUEST_TIMEOUT);
    std::unique_ptr<zmq::message_t> reply = std::make_unique<zmq::message_t>();
    zmq::recv_result_t recv_result;
    if (items[0].revents & ZMQ_POLLIN) recv_result = client.recv(*reply, zmq::recv_flags::none);
    if (!recv_result) {
        reply.release();
    }
    return reply;
}

//  .split client task
//  The client uses a Lazy Pirate strategy if it only has one server to talk
//  to. If it has two or more servers to talk to, it will try each server just
//  once:

int main(int argc, char *argv[]) {
    zmq::context_t context{1};
    zmq::const_buffer request = zmq::str_buffer("Hello World!");

    std::unique_ptr<zmq::message_t> reply;

    int endpoints = argc - 1;

    if (endpoints == 0)
        std::cout << "I: syntax: " << argv[0] << "<endpoint> ..." << std::endl;
    else if (endpoints == 1) {
        //  For one endpoint, we retry N times
        int retries;
        for (retries = 0; retries < MAX_RETRIES; retries++) {
            std::string endpoint = std::string(argv[1]);
            reply = s_try_request(context, endpoint, request);
            if (reply) break;  //  Successful
            std::cout << "W: no response from " << endpoint << " retrying...\n" << std::endl;
        }
    } else {
        //  For multiple endpoints, try each at most once
        int endpoint_nbr;
        for (endpoint_nbr = 0; endpoint_nbr < endpoints; endpoint_nbr++) {
            std::string endpoint = std::string(argv[endpoint_nbr + 1]);
            reply = s_try_request(context, endpoint, request);
            if (reply) break;  //  Successful
            std::cout << "W: no response from " << endpoint << std::endl;
        }
    }
    if (reply)
        std::cout << "Service is running OK. Received message: " << reply->to_string() << std::endl;
    return 0;
}
