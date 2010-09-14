//
//  Durable subscriber
//
// Olivier Chamoux <olivier.chamoux@fr.thalesgroup.com>

#include "zhelpers.hpp"

int main (int argc, char *argv[])
{
    zmq::context_t context(1);

    //  Connect our subscriber socket
    zmq::socket_t subscriber (context, ZMQ_SUB);
    subscriber.setsockopt(ZMQ_IDENTITY, "Hello", 5);
    subscriber.setsockopt(ZMQ_SUBSCRIBE, "", 0);
    subscriber.connect("tcp://localhost:5565");

    //  Synchronize with publisher
    zmq::socket_t sync (context, ZMQ_PUSH);
    sync.connect("tcp://localhost:5564");
    s_send (sync, "");

    //  Get updates, expect random Ctrl-C death
    while (1) {
        std::string *string = s_recv (subscriber);
        std::cout << *string << std::endl;
        
        if (string->compare("END") == 0) {
            delete (string);
            break;
        }
        delete (string);
    }
    return 0;
}
