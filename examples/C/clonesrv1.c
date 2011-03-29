//
//  Clone server model 1
//
//  Lets us 'build clonesrv1' and 'build all'
#include "kvmsg.c"

int main (void) 
{
    //  Prepare our context and publisher socket
    void *context = zmq_init (1);
    void *publisher = zmq_socket (context, ZMQ_PUB);
    zmq_bind (publisher, "tcp://*:5556");
    s_sleep (200);

    s_catch_signals ();
    zhash_t *kvmap = zhash_new ();
    int64_t sequence = 0;
    srandom ((unsigned) time (NULL));

    while (!s_interrupted) {
        //  Distribute as key-value message
        kvmsg_t *kvmsg = kvmsg_new (++sequence);
        kvmsg_fmt_key  (kvmsg, "%d", randof (10000));
        kvmsg_fmt_body (kvmsg, "%d", randof (1000000));
        kvmsg_send (kvmsg, publisher);
        kvmsg_store (&kvmsg, kvmap);
    }
    printf (" Interrupted\n%" PRId64 " messages out\n", sequence);
    zhash_destroy (&kvmap);
    zmq_close (publisher);
    zmq_term (context);
    return 0;
}
