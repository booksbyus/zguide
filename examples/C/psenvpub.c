//
//  Pubsub envelope publisher
//  Note that the zhelpers.h file also provides s_sendmore
//
#include "zhelpers.h"

int main (void)
{
    //  Prepare our context and publisher
    void *context = zmq_init (1);
    void *publisher = zmq_socket (context, ZMQ_PUB);
    zmq_bind (publisher, "tcp://*:5563");

    while (1) {
        //  Write two messages, each with an envelope and content
        s_sendmore (publisher, "A");
        s_send (publisher, "We don't want to see this");
        s_sendmore (publisher, "B");
        s_send (publisher, "We would like to see this");
        sleep (1);
    }
    //  We never get here but clean up anyhow
    zmq_close (publisher);
    zmq_term (context);
    return 0;
}
