//  Pathological publisher
//  Sends out 1,000 topics and then one random update per second

#include <thread>
#include <chrono>
#include "zhelpers.hpp"

int main (int argc, char *argv [])
{
    zmq::context_t context(1);
    zmq::socket_t publisher(context, ZMQ_PUB);

    //  Initialize random number generator
    srandom ((unsigned) time (NULL));

    if (argc == 2)
        publisher.bind(argv [1]);
    else
        publisher.bind("tcp://*:5556");

    //  Ensure subscriber connection has time to complete
    std::this_thread::sleep_for(std::chrono::seconds(1));

    //  Send out all 1,000 topic messages
    int topic_nbr;
    for (topic_nbr = 0; topic_nbr < 1000; topic_nbr++) {
        std::stringstream ss;
        ss << std::dec << std::setw(3) << std::setfill('0') << topic_nbr;

        s_sendmore (publisher, ss.str());
        s_send (publisher, "Save Roger");
    }

    //  Send one random update per second
    while (1) {

        std::this_thread::sleep_for(std::chrono::seconds(1));
        std::stringstream ss;
        ss << std::dec << std::setw(3) << std::setfill('0') << within(1000);

        s_sendmore (publisher, ss.str());
        s_send (publisher, "Off with his head!");
    }
    return 0;
}
