//
//  Broker peering simulation (part 1)
//  Prototypes the state flow
//
#include "zhelpers.h"
#include "zmsg.c"

int main (int argc, char *argv[])
{
    //  First argument is this cluster's name
    //  Other arguments are our peers' names
    //
    if (argc < 3) {
        printf ("syntax: peering1 me other1 other2...\n");
        exit (EXIT_FAILURE);
    }
    char *self = argv [1];
    printf ("I: preparing broker at %s...\n", self);
    srandom ((unsigned) time (NULL));

    //  Prepare our context and sockets
    void *context = zmq_init (1);
    char endpoint [256];

    //  Bind statebe to endpoint
    void *statebe = zmq_socket (context, ZMQ_PUB);
    snprintf (endpoint, 255, "ipc://%s-state.ipc", self);
    assert (zmq_bind (statebe, endpoint) == 0);

    //  Connect statefe to all peers
    void *statefe = zmq_socket (context, ZMQ_SUB);
    zmq_setsockopt (statefe, ZMQ_SUBSCRIBE, "", 0);
    int argn;
    for (argn = 2; argn < argc; argn++) {
        char *peer = argv [argn];
        printf ("I: connecting to state backend at '%s'\n", peer);
        snprintf (endpoint, 255, "ipc://%s-state.ipc", peer);
        assert (zmq_connect (statefe, endpoint) == 0);
    }
    //  Send out status messages to peers, and collect from peers
    //  The zmq_poll timeout defines our own heartbeating
    while (1) {
        //  Initialize poll set
        zmq_pollitem_t items [] = {
            { statefe, 0, ZMQ_POLLIN, 0 }
        };
        //  Poll for activity, or 1 second timeout
        assert (zmq_poll (items, 1, 1000000) >= 0);

        //  Handle incoming status message
        if (items [0].revents & ZMQ_POLLIN) {
            zmsg_t *zmsg = zmsg_recv (statefe);
            printf ("%s - %s workers free\n",
                zmsg_address (zmsg), zmsg_body (zmsg));
            zmsg_destroy (&zmsg);
        }
        else {
            //  Send random value for worker availability
            zmsg_t *zmsg = zmsg_new ();
            zmsg_body_fmt (zmsg, "%d", within (10));
            //  We stick our own address onto the envelope
            zmsg_wrap (zmsg, self, NULL);
            zmsg_send (&zmsg, statebe);
        }
    }
    zmq_term (context);
    return 0;
}
