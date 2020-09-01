//
//  Synchronized subscriber in C++
//

#include "zhelpers.hpp"

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
    s_send (syncclient, "");

    //  - wait for synchronization reply
    s_recv (syncclient);

    //  Third, get our updates and report how many we got
    int update_nbr = 0;
    while (1) {

        if (s_recv (subscriber).compare("END") == 0) {
            break;
        }

        update_nbr++;
    }
    std::cout << "Received " << update_nbr << " updates" << std::endl;

    return 0;
}
