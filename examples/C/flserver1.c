//
//  Freelance server - Model 1
//  Trivial echo service
//
#include "zmsg.h"

int main (int argc, char *argv [])
{
    if (argc < 2) {
        printf ("I: syntax: %s <endpoint>\n", argv [0]);
        exit (EXIT_SUCCESS);
    }
    void *context = zmq_init (1);
    s_catch_signals ();

    //  Implement basic echo service
    void *server = zmq_socket (context, ZMQ_REP);
    zmq_bind (server, argv [1]);
    printf ("I: echo service is ready at %s\n", argv [1]);
    while (!s_interrupted) {
        zmsg_t *msg = zmsg_recv (server);
        if (!msg) 
            break;          //  Interrupted
        zmsg_send (&msg, server);
    }
    if (s_interrupted)
        printf ("W: interrupted\n");

    zmq_close (server);
    zmq_term (context);
    return 0;
}
