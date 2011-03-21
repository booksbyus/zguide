//
//  Clone client model 2
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
    
    //  Get first update, then get a state snapshot
    kvmsg_t *update = kvmsg_recv (subscriber);
    
    void *snapshot = zmq_socket (context, ZMQ_XREQ);
    zmq_connect (snapshot, "tcp://localhost:5557");
    s_send (snapshot, "I can haz state?");
    
    //  Get all state messages until interrupted or done
    int64_t sequence = 0;
    while (!s_interrupted) {
        kvmsg_t *kvmsg = kvmsg_recv (snapshot);
        if (!kvmsg)
            break;          //  Interrupted
        if (strcmp (kvmsg_key (kvmsg), "KTHXBAI") == 0) {
            sequence = kvmsg_sequence (kvmsg);
            kvmsg_destroy (&kvmsg);
            break;          //  Done
        }
        kvmsg_store (kvmsg, kvmap);
    }
    printf ("Received sequence=%" PRId64 "\n", sequence);
    
    //  We'll need to cancel the pending message we were interrupted
    int zero = 0;
    zmq_setsockopt (snapshot, ZMQ_LINGER, &zero, sizeof (zero));
    zmq_close (snapshot);
    
    //  Now apply pending updates from (sequence + 1)
    while (update && !s_interrupted) {
        if (kvmsg_sequence (update) <= sequence)
            kvmsg_destroy (&update);
        else
            kvmsg_store (update, kvmap);
        update = kvmsg_recv (subscriber);
    }
    zhash_destroy (&kvmap);
    zmq_close (subscriber);
    zmq_term (context);
    return 0;
}
