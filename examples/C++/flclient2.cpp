//  Freelance client - Model 2
//  Uses DEALER socket to blast one or more services

#include <chrono>
#include <iostream>
#include <memory>
#include <zmqpp/zmqpp.hpp>

//  If not a single service replies within this time, give up
const int GLOBAL_TIMEOUT = 2500;
//  Total requests times
const int TOTAL_REQUESTS = 10000;

//  .split class implementation
//  Here is the {{flclient}} class implementation. Each instance has a
//  context, a DEALER socket it uses to talk to the servers, a counter
//  of how many servers it's connected to, and a request sequence number:
class flclient {
   public:
    flclient();
    ~flclient() {}
    void connect(const std::string &endpoint);
    std::unique_ptr<zmqpp::message> request(zmqpp::message &request);

   private:
    zmqpp::context context_;  //  Our context
    zmqpp::socket socket_;    //  DEALER socket talking to servers
    size_t servers_;          //  How many servers we have connected to
    uint sequence_;           //  Number of requests ever sent
};

//  Constructor
flclient::flclient() : socket_(context_, zmqpp::socket_type::dealer) {
    socket_.set(zmqpp::socket_option::linger, GLOBAL_TIMEOUT);
    servers_ = 0;
    sequence_ = 0;
}

//  Connect to new server endpoint
void flclient::connect(const std::string &endpoint) {
    socket_.connect(endpoint);
    servers_++;
}

//  .split request method
//  This method does the hard work. It sends a request to all
//  connected servers in parallel (for this to work, all connections
//  must be successful and completed by this time). It then waits
//  for a single successful reply, and returns that to the caller.
//  Any other replies are just dropped:

std::unique_ptr<zmqpp::message> flclient::request(zmqpp::message &request) {
    //  Prefix request with sequence number and empty envelope
    request.push_front(++sequence_);
    request.push_front("");

    //  Blast the request to all connected servers
    size_t server;
    for (server = 0; server < servers_; server++) {
        zmqpp::message msg;
        msg.copy(request);
        socket_.send(msg);
    }
    //  Wait for a matching reply to arrive from anywhere
    //  Since we can poll several times, calculate each one
    std::unique_ptr<zmqpp::message> reply;

    zmqpp::poller poller;
    poller.add(socket_, zmqpp::poller::poll_in);
    auto endTime = std::chrono::system_clock::now() + std::chrono::milliseconds(GLOBAL_TIMEOUT);
    while (std::chrono::system_clock::now() < endTime) {
        int milliSecondsToWait = std::chrono::duration_cast<std::chrono::milliseconds>(
                                     endTime - std::chrono::system_clock::now())
                                     .count();
        if (poller.poll(milliSecondsToWait)) {
            if (poller.has_input(socket_)) {
                reply = std::make_unique<zmqpp::message>();
                //  Reply is [empty][sequence][OK]
                socket_.receive(*reply);
                assert(reply->parts() == 3);
                reply->pop_front();
                uint sequence;
                reply->get(sequence, 0);
                reply->pop_front();
                // std::cout << "Current sequence: " << sequence_ << ", Server reply: " << sequence
                //           << std::endl;
                if (sequence == sequence_)
                    break;
                else
                    reply.release();
            }
        }
    }
    return reply;
}

int main(int argc, char *argv[]) {
    if (argc == 1) {
        std::cout << "I: syntax: " << argv[0] << " <endpoint> ..." << std::endl;
        return 0;
    }
    //  Create new freelance client object
    flclient client;

    //  Connect to each endpoint
    int argn;
    for (argn = 1; argn < argc; argn++) client.connect(argv[argn]);

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
