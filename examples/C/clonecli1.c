//
//  Clone client model 1
//
#include "kvmsg.h"

int main (void)
{
    //  Prepare our context and subscriber
    void *context = zmq_init (1);
    void *subscriber = zmq_socket (context, ZMQ_SUB);
    zmq_setsockopt (subscriber, ZMQ_SUBSCRIBE, "", 0);
    zmq_connect (subscriber, "tcp://localhost:5556");

    s_catch_signals ();
    zhash_t *kvmap = zhash_new ();
    int64_t sequence = 0;

    while (!s_interrupted) {
        kvmsg_t *kvmsg = kvmsg_recv (subscriber);
        if (!kvmsg)
            break;          //  Interrupted
        kvmsg_hash_put (kvmsg, kvmap);
        kvmsg_destroy (&kvmsg);
        sequence++;
    }
    printf (" Interrupted\n%" PRId64 " messages in\n", sequence);
    zhash_destroy (&kvmap);
    zmq_close (subscriber);
    zmq_term (context);
    return 0;
}
