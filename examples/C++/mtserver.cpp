/*
author: Saad Hussain <saadnasir31@gmail.com>
date: 30th January 2024
*/

#include <string>
#include <iostream>
#include <thread>
#include <zmq.hpp>

void worker_routine(zmq::context_t& ctx) {
    zmq::socket_t socket(ctx, ZMQ_REP);
    socket.connect("inproc://workers");

    while(true) {
        zmq::message_t request;
        socket.recv(&request);
        std::cout << "Received request: [" << (char*) request.data() << "]" << std::endl;
        std::this_thread::sleep_for(std::chrono::seconds(1));

        zmq::message_t reply("World", 5);
        socket.send(reply);
    }
}

int main() {
    zmq::context_t ctx(1);
    zmq::socket_t clients(ctx, ZMQ_ROUTER);
    clients.bind("tcp://localhost:5555");

    zmq::socket_t workers(ctx, ZMQ_DEALER);
    workers.bind("inproc://workers");
    
    std::vector<std::thread> worker_threads;
    for (int thread_nbr = 0; thread_nbr != 5; ++thread_nbr) {
        worker_threads.emplace_back([&ctx] { worker_routine(ctx); }); 
    }

    zmq::proxy(clients, workers, nullptr);

    for (auto& thread : worker_threads) {
        thread.join();
    }
    
    return 0;

}
