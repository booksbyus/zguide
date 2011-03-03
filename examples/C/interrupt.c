//
//  Shows how to handle Ctrl-C
//
#include "zhelpers.h"

int main (void)
{
    void *context = zmq_init (1);
    void *server = zmq_socket (context, ZMQ_REP);
    zmq_bind (server, "tcp://*:5555");

    s_catch_signals ();
    while (1) {
        //  Blocking read will exit on a signal
        char *request = s_recv (server);
        if (s_interrupted) {
            printf ("W: interrupt received, killing server...\n");
            break;
        }
        free (request);
        s_send (server, "World");
    }
    zmq_close (server);
    zmq_term (context);
    return 0;
}
