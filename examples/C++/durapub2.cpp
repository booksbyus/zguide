//
//  Publisher for durable subscriber
//
// Olivier Chamoux <olivier.chamoux@fr.thalesgroup.com>

#include "zhelpers.hpp"

#include <stdint.h>

int main () {
    zmq::context_t context(1);

    //  Subscriber tells us when it's ready here
    zmq::socket_t sync(context, ZMQ_PULL);
    sync.bind("tcp://*:5564");

    //  We send updates via this socket
    zmq::socket_t publisher (context, ZMQ_PUB);
    publisher.bind("tcp://*:5565");

    //  Prevent publisher overflow from slow subscribers
    uint64_t hwm = 1;
    publisher.setsockopt( ZMQ_HWM, &hwm, sizeof (hwm));

    //  Specify swap space in bytes, this covers all subscribers
    uint64_t swap = 25000000;
    publisher.setsockopt( ZMQ_SWAP, &swap, sizeof (swap));

    //  Wait for synchronization request
    s_recv (sync);

    //  Now broadcast exactly 10 updates with pause
    int update_nbr;
    for (update_nbr = 0; update_nbr < 10; update_nbr++) {
       
        std::ostringstream oss;
        oss << "Update "<< update_nbr ;
        s_send (publisher, oss.str());
        sleep (1);
    }
    s_send (publisher, "END");

    sleep (1);              //  Give 0MQ/2.0.x time to flush output
    return 0;
}
