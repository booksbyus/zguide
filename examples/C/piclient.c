//
//  Client-side pirate
//  Use zmq_poll to do a safe request-reply
//  To run, start hwserver and then randomly kill/restart it
//
#include "zhelpers.h"

#define REQUEST_TIMEOUT     2500    //  msecs
#define REQUEST_RETRIES     3       //  Before we abandon

int main () {
    void *context = zmq_init (1);
    void *client = zmq_socket (context, ZMQ_REQ);
    zmq_connect (client, "tcp://localhost:5555");

    //  Socket to talk to server
    printf ("Connecting to hello world server...\n");

    int retries_left = REQUEST_RETRIES;
    while (1) {
        s_send (client, "HELLO");

        zmq_pollitem_t items [] = { { client, 0, ZMQ_POLLIN, 0 } };
        zmq_poll (items, 1, REQUEST_TIMEOUT * 1000);

        if (items [0].revents & ZMQ_POLLIN) {
            //  We got a reply from the server
            char *reply = s_recv (client);
            free (reply);
            printf ("Server replied\n");
            retries_left = REQUEST_RETRIES;
        }
        else {
            //  No response from server
            if (--retries_left == 0) {
                printf ("E: server seems to be offline, abandoning\n");
                break;
            }
            else {
                printf ("W: no response from server, retrying...\n");
                //  Create a new socket, old one will be confused
                zmq_close (client);
                client = zmq_socket (context, ZMQ_REQ);
                zmq_connect (client, "tcp://localhost:5555");
            }
        }
    }
    //  Discard any outgoing requests
    int linger = 0;
    zmq_setsockopt (client, ZMQ_LINGER, &linger, sizeof (linger));
    
    zmq_close (client);
    zmq_term (context);
    return 0;
}
