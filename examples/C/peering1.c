//
//  Broker peering simulation (part 1)
//  Prototypes the state flow
//
#include "zhelpers.h"

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

    //  Prepare our context and sockets
    void *context = zmq_init (1);
    char endpoint [256];

    //  Bind statebe to endpoint, connect statefe to all peers
    void *statebe = zmq_socket (context, ZMQ_PUB);
    snprintf (endpoint, 255, "ipc://%s-state.ipc", self);
    assert (zmq_bind (statebe, endpoint) == 0);

    int argn;
    void *statefe = zmq_socket (context, ZMQ_SUB);
    zmq_setsockopt (statefe, ZMQ_SUBSCRIBE, "", 0);
    for (argn = 2; argn < argc; argn++) {
        char *peer = argv [argn];
        printf ("I: peering with broker at '%s'\n", peer);
        snprintf (endpoint, 255, "ipc://%s-state.ipc", peer);
        assert (zmq_connect (statefe, endpoint) == 0);
    }
    //  Send out status messages to peers, and collect from peers
    //  The zmq_poll timeout defines our own heartbeating
    while (1) {
        //  Initialize poll set
        zmq_pollitem_t items [1] = {
            { statefe, 0, ZMQ_POLLIN, 0 }
        };
        //  Poll for activity, or 1 second timeout
        assert (zmq_poll (items, 1, 1000000) >= 0);

        //  Handle incoming status message
        if (items [0].revents & ZMQ_POLLIN) {
            //  Get address of peer broker
            char *broker_addr = s_recv (statefe);
            //  Get number of available workers
            char *broker_status = s_recv (statefe);

            printf ("%s - %s workers free\n", broker_addr, broker_status);
            free (broker_addr);
            free (broker_status);
        }
        else {
            //  Send message envelope and body
            //  Here, just some random value for worker availability
            char status [3];
            snprintf (status, 2, "%d", within (10));
            s_sendmore (statebe, self);
            s_send     (statebe, status);
        }
    }
    zmq_term (context);
    return 0;
}
