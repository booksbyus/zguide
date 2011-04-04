//
//  Clone client model 1
//

//  Lets us build this source without creating a library
#include "kvmsg.c"

int main (void)
{
    //  Prepare our context and updates socket
    zctx_t *ctx = zctx_new ();
    void *updates = zctx_socket_new (ctx, ZMQ_SUB);
    zmq_setsockopt (updates, ZMQ_SUBSCRIBE, "", 0);
    zmq_connect (updates, "tcp://localhost:5556");

    zhash_t *kvmap = zhash_new ();
    int64_t sequence = 0;

    while (!zctx_interrupted) {
        kvmsg_t *kvmsg = kvmsg_recv (updates);
        if (!kvmsg)
            break;          //  Interrupted
        kvmsg_store (&kvmsg, kvmap);
        sequence++;
    }
    printf (" Interrupted\n%" PRId64 " messages in\n", sequence);
    zhash_destroy (&kvmap);
    zctx_socket_destroy (ctx, updates);
    zctx_destroy (zmq_term (context)ctx);
    return 0;
}
