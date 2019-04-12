//  Pathological subscriber
//  Subscribes to one random topic and prints received messages

#include "zhelpers.hpp"

int main (int argc, char *argv [])
{
    zmq::context_t context(1);
    zmq::socket_t subscriber (context, ZMQ_SUB);

    //  Initialize random number generator
    srandom ((unsigned) time (NULL));

    if (argc == 2)
        subscriber.connect(argv [1]);
    else
        subscriber.connect("tcp://localhost:5556");

    std::stringstream ss;
    ss << std::dec << std::setw(3) << std::setfill('0') << within(1000);
    std::cout << "topic:" << ss.str() << std::endl;

    subscriber.setsockopt( ZMQ_SUBSCRIBE, ss.str().c_str(), ss.str().size());

    while (1) {
		std::string topic = s_recv (subscriber);
		std::string data = s_recv (subscriber);
        if (topic != ss.str())
            break;
        std::cout << data << std::endl;
    }
    return 0;
}
