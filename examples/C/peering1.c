//
//  Broker peering simulation (part 1)
//  Prototypes the state flow
//
#include "zapi.h"

int main (int argc, char *argv [])
{
    //  First argument is this broker's name
    //  Other arguments are our peers' names
    //
    if (argc < 2) {
        printf ("syntax: peering1 me {you}...\n");
        exit (EXIT_FAILURE);
    }
    char *self = argv [1];
    printf ("I: preparing broker at %s...\n", self);
    srandom ((unsigned) time (NULL));

    //  Prepare our context and sockets
    zctx_t *ctx = zctx_new ();

    //  Bind statebe to endpoint
    void *statebe = zctx_socket_new (ctx, ZMQ_PUB);
    char endpoint [256];
    snprintf (endpoint, 255, "ipc://%s-state.ipc", self);
    int rc = zmq_bind (statebe, endpoint);
    assert (rc == 0);

    //  Connect statefe to all peers
    void *statefe = zctx_socket_new (ctx, ZMQ_SUB);
    zmq_setsockopt (statefe, ZMQ_SUBSCRIBE, "", 0);

    int argn;
    for (argn = 2; argn < argc; argn++) {
        char *peer = argv [argn];
        printf ("I: connecting to state backend at '%s'\n", peer);
        snprintf (endpoint, 255, "ipc://%s-state.ipc", peer);
        rc = zmq_connect (statefe, endpoint);
        assert (rc == 0);
    }
    //  Send out status messages to peers, and collect from peers
    //  The zmq_poll timeout defines our own heartbeating
    //
    while (1) {
        //  Initialize poll set
        zmq_pollitem_t items [] = {
            { statefe, 0, ZMQ_POLLIN, 0 }
        };
        //  Poll for activity, or 1 second timeout
        rc = zmq_poll (items, 1, 1000000);
        if (rc == -1)
            break;              //  Interrupted

        //  Handle incoming status message
        if (items [0].revents & ZMQ_POLLIN) {
            char *peer_name = zstr_recv (statefe);
            char *available = zstr_recv (statefe);
            printf ("%s - %s workers free\n", peer_name, available);
            free (peer_name);
            free (available);
        }
        else {
            //  Send random value for worker availability
            zstr_sendm (statebe, self);
            zstr_sendf (statebe, "%d", randof (10));
        }
    }
    zctx_destroy (&ctx);
    return EXIT_SUCCESS;
}
