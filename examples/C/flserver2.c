//
//  Freelance server - Model 2
//  Does some work, replies OK, with message sequencing
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

    void *server = zmq_socket (context, ZMQ_REP);
    zmq_bind (server, argv [1]);
    printf ("I: service is ready at %s\n", argv [1]);
    while (!s_interrupted) {
        zmsg_t *msg = zmsg_recv (server);
        if (!msg) 
            break;          //  Interrupted
        //  Fail nastily if run against wrong client
        assert (zmsg_parts (msg) == 2);
        
        zmsg_body_set (msg, "OK");
        zmsg_send (&msg, server);
    }
    if (s_interrupted)
        printf ("W: interrupted\n");

    zmq_close (server);
    zmq_term (context);
    return 0;
}
