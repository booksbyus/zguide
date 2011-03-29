//
//  Clone client model 2
//
//  Lets us 'build clonecli2' and 'build all'
#include "kvmsg.c"

int main (void)
{
    //  Prepare our context and subscriber
    void *context = zmq_init (1);
    void *subscriber = zmq_socket (context, ZMQ_SUB);
    zmq_setsockopt (subscriber, ZMQ_SUBSCRIBE, "", 0);
    zmq_connect (subscriber, "tcp://localhost:5556");

    void *snapshot = zmq_socket (context, ZMQ_XREQ);
    zmq_connect (snapshot, "tcp://localhost:5557");

    s_catch_signals ();
    zhash_t *kvmap = zhash_new ();

    //  Get state snapshot
    int64_t sequence = 0;
    s_send (snapshot, "I can haz state?");
    while (!s_interrupted) {
        kvmsg_t *kvmsg = kvmsg_recv (snapshot);
        if (!kvmsg)
            break;          //  Interrupted
        if (streq (kvmsg_key (kvmsg), "KTHXBAI")) {
            sequence = kvmsg_sequence (kvmsg);
            kvmsg_destroy (&kvmsg);
            break;          //  Done
        }
        kvmsg_store (&kvmsg, kvmap);
    }
    printf ("Received snapshot=%" PRId64 "\n", sequence);
    
    int zero = 0;
    zmq_setsockopt (snapshot, ZMQ_LINGER, &zero, sizeof (zero));
    zmq_close (snapshot);
    
    //  Now apply pending updates, discard out-of-sequence messages
    while (!s_interrupted) {
        kvmsg_t *kvmsg = kvmsg_recv (subscriber);
        if (!kvmsg)
            break;          //  Interrupted
        if (kvmsg_sequence (kvmsg) > sequence) {
            sequence = kvmsg_sequence (kvmsg);
            kvmsg_store (&kvmsg, kvmap);
        }
        else
            kvmsg_destroy (&kvmsg);
    }
    zhash_destroy (&kvmap);
    zmq_close (subscriber);
    zmq_term (context);
    return 0;
}
