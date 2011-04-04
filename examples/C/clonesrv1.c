//
//  Clone server model 1
//

//  Lets us build this source without creating a library
#include "kvmsg.c"

int main (void) 
{
    //  Prepare our context and publisher socket
    zctx_t *ctx = zctx_new ();
    void *publisher = zctx_socket_new (ctx, ZMQ_PUB);
    zmq_bind (publisher, "tcp://*:5556");
    zclock_sleep (200);

    zhash_t *kvmap = zhash_new ();
    int64_t sequence = 0;
    srandom ((unsigned) time (NULL));

    while (!zctx_interrupted) {
        //  Distribute as key-value message
        kvmsg_t *kvmsg = kvmsg_new (++sequence);
        kvmsg_fmt_key  (kvmsg, "%d", randof (10000));
        kvmsg_fmt_body (kvmsg, "%d", randof (1000000));
        kvmsg_send (kvmsg, publisher);
        kvmsg_store (&kvmsg, kvmap);
    }
    printf (" Interrupted\n%" PRId64 " messages out\n", sequence);
    zhash_destroy (&kvmap);
    zctx_socket_destroy (ctx, publisher);
    zctx_destroy (zmq_term (context)ctx);
    return 0;
}
