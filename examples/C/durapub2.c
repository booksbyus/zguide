//
//  Publisher for durable subscriber
//
#include "zhelpers.h"

int main () {
    void *context = zmq_init (1);

    //  Subscriber tells us when it's ready here
    void *sync = zmq_socket (context, ZMQ_PULL);
    zmq_bind (sync, "tcp://*:5564");

    //  We send updates via this socket
    void *publisher = zmq_socket (context, ZMQ_PUB);
    zmq_bind (publisher, "tcp://*:5565");

    //  Prevent publisher overflow from slow subscribers
    uint64_t hwm = 1;
    zmq_setsockopt (publisher, ZMQ_HWM, &hwm, sizeof (hwm));

    //  Specify swap space in bytes, this covers all subscribers
    uint64_t swap = 25000000;
    zmq_setsockopt (publisher, ZMQ_SWAP, &swap, sizeof (swap));

    //  Wait for synchronization request
    char *string = s_recv (sync);
    free (string);

    //  Now broadcast exactly 10 updates with pause
    int update_nbr;
    for (update_nbr = 0; update_nbr < 10; update_nbr++) {
        char string [20];
        sprintf (string, "Update %d", update_nbr);
        s_send (publisher, string);
        sleep (1);
    }
    s_send (publisher, "END");

    sleep (1);              //  Give 0MQ/2.0.x time to flush output
    return 0;
}
