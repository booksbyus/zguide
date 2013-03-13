//  Decentralized chat example
//
#include <czmq.h>

static void
listener_task (void *args, zctx_t *ctx, void *pipe)
{
    void *listener = zsocket_new (ctx, ZMQ_SUB);
    int address;
    for (address = 1; address < 255; address++) {
        int rc = zsocket_connect (listener,
            "tcp://%s%d:9000", (char *) args, address);
        assert (rc == 0);
    }
    zsocket_set_subscribe (listener, "");
    while (!zctx_interrupted) {
        char *message = zstr_recv (listener);
        if (message) {
            printf ("%s", message);
            free (message);
        }
    }
}

int main (int argc, char *argv [])
{
    char *dot = argc == 4? strrchr (argv [1], '.'): NULL;
    if (dot)
        dot [1] = 0;        //  Cut string after dot
    else {
        puts ("Usage: dechat ipaddress interface username");
        puts ("Example: dechat 192.168.55.123 eth0 joe");
        exit (0);
    }
    zctx_t *ctx = zctx_new ();
    zthread_fork (ctx, listener_task, argv [1]);

    void *broadcaster = zsocket_new (ctx, ZMQ_PUB);
    zsocket_bind (broadcaster, "tcp://%s:9000", argv [2]);
    while (!zctx_interrupted) {
        char message [1024];
        if (!fgets (message, 1024, stdin))
            break;
        zstr_send (broadcaster, "%s: %s", argv [3], message);
    }
    zctx_destroy (&ctx);
    return 0;
}
