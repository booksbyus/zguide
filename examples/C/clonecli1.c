//
//  Clone client model 1
//
#include "kvmsg.h"

int main (void)
{
    //  Prepare our context and updates socket
    void *context = zmq_init (1);
    void *updates = zmq_socket (context, ZMQ_SUB);
    zmq_setsockopt (updates, ZMQ_SUBSCRIBE, "", 0);
    zmq_connect (updates, "tcp://localhost:5556");

    s_catch_signals ();
    zhash_t *kvmap = zhash_new ();
    int64_t sequence = 0;

    while (!s_interrupted) {
        kvmsg_t *kvmsg = kvmsg_recv (updates);
        if (!kvmsg)
            break;          //  Interrupted
        kvmsg_store (&kvmsg, kvmap);
        sequence++;
    }
    printf (" Interrupted\n%" PRId64 " messages in\n", sequence);
    zhash_destroy (&kvmap);
    zmq_close (updates);
    zmq_term (context);
    return 0;
}
