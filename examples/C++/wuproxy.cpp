//
//  Weather proxy device C++
//

#include "zhelpers.hpp"

int main (int argc, char *argv[])
{
    zmq::context_t context(1);

    //  This is where the weather server sits
    zmq::socket_t frontend(context, ZMQ_SUB);
    frontend.connect("tcp://192.168.55.210:5556");

    //  This is our public endpoint for subscribers
    zmq::socket_t backend (context, ZMQ_PUB);
    backend.bind("tcp://10.1.1.0:8100");

    //  Subscribe on everything
    frontend.setsockopt(ZMQ_SUBSCRIBE, "", 0);

    //  Shunt messages out to our own subscribers
    while (1) {
        while (1) {
            zmq::message_t message;
            int more;
            size_t more_size = sizeof (more);

            //  Process all parts of the message
            frontend.recv(&message);
            frontend.getsockopt( ZMQ_RCVMORE, &more, &more_size);
            backend.send(message, more? ZMQ_SNDMORE: 0);
            if (!more)
                break;      //  Last message part
        }
    }
    return 0;
}
