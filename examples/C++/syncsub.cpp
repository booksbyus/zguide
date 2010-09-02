//
//  Synchronized subscriber in C++
//
//  Olivier Chamoux <olivier.chamoux@fr.thalesgroup.com>
//
#include <zmq.hpp>
#include <iostream>

int main (int argc, char *argv[])
{
   zmq::context_t context(1);

    //  First, connect our subscriber socket
    zmq::socket_t subscriber (context, ZMQ_SUB);
    subscriber.connect("tcp://localhost:5561");
    subscriber.setsockopt(ZMQ_SUBSCRIBE, "", 0);

    //  Second, synchronize with publisher
    zmq::socket_t syncclient (context, ZMQ_REQ);
    syncclient.connect("tcp://localhost:5562");

    //  - send a synchronization request
    zmq::message_t request;
    syncclient.send(request);

    //  - wait for synchronization reply
    zmq::message_t reply;
    syncclient.recv(&reply);

    //  Third, get our updates and report how many we got
    int update_nbr = 0;
    while (1) {
        zmq::message_t update;
        subscriber.recv(&update);

        if (update.size() == 4
        && memcmp (update.data(), "END\0", 4) == 0)
            break;

        update_nbr++;
    }
    std::cout << "Received " << update_nbr << " updates" << std::endl;

    return 0;
}
