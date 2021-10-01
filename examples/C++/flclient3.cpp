//  Freelance client - Model 3
//  Uses flcliapi class to encapsulate Freelance pattern

#include <chrono>

#include "flcliapi.hpp"

const int TOTAL_REQUESTS = 10000;

int main(void) {
    //  Create new freelance client object
    Flcliapi client;

    //  Connect to several endpoints
    client.connect("tcp://localhost:5555");
    client.connect("tcp://localhost:5556");
    client.connect("tcp://localhost:5557");

    //  Send a bunch of name resolution 'requests', measure time
    int requests = TOTAL_REQUESTS;
    auto startTime = std::chrono::steady_clock::now();
    while (requests--) {
        zmqpp::message request;
        request.push_back("random name");
        std::unique_ptr<zmqpp::message> reply;
        reply = client.request(request);
        if (!reply) {
            std::cout << "E: name service not available, aborting" << std::endl;
            break;
        }
    }
    auto endTime = std::chrono::steady_clock::now();
    std::cout
        << "Average round trip cost: "
        << std::chrono::duration_cast<std::chrono::microseconds>(endTime - startTime).count() /
               TOTAL_REQUESTS
        << " µs" << std::endl;
    return 0;
}
