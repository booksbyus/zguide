//  Shows how to provoke EAGAIN when reaching HWM

#include <czmq.h>

int main (void) {
    zctx_t *ctx = zctx_new ();
    
    void *mailbox = zsocket_new (ctx, ZMQ_DEALER);
    zsocket_set_sndhwm (mailbox, 4);
    zsocket_set_sndtimeo (mailbox, 0);
    zsocket_connect (mailbox, "tcp://localhost:9876");

    int count;
    for (count = 0; count < 10; count++) {
        printf ("Sending message %d\n", count);
        int rc = zstr_send (mailbox, "message %d", count);
        if (rc == -1) {
            printf ("%s\n", strerror (errno));
            break;
        }
    }
    zctx_destroy (&ctx);
    return 0;
}
