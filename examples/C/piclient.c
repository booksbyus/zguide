//
//  Client-side pirate
//  Use zmq_poll to do a safe request-reply
//  To run, start piserver and then randomly kill/restart it
//
#include "zhelpers.h"

#define REQUEST_TIMEOUT     2500    //  msecs, (> 1000!)
#define REQUEST_RETRIES     3       //  Before we abandon

//  Helper function that returns a new configured socket
//  connected to the Hello World server
//
static void *
s_client_socket (void *context) {
    printf ("I: connecting to server...\n");
    void *client = zmq_socket (context, ZMQ_REQ);
    zmq_connect (client, "tcp://localhost:5555");

    //  Configure socket to not wait at close time
    int linger = 0;
    zmq_setsockopt (client, ZMQ_LINGER, &linger, sizeof (linger));
    return client;
}

int main (void)
{
    s_version_assert (2, 1);
    void *context = zmq_init (1);
    void *client = s_client_socket (context);

    int sequence = 0;
    int retries_left = REQUEST_RETRIES;
    while (retries_left) {
        //  We send a request, then we work to get a reply
        char request [10];
        sprintf (request, "%d", ++sequence);
        //  Wait one second in between requests
        sleep (1);
        s_send (client, request);

        int expect_reply = 1;
        while (expect_reply) {
            //  Poll socket for a reply, with timeout
            zmq_pollitem_t items [] = { { client, 0, ZMQ_POLLIN, 0 } };
            zmq_poll (items, 1, REQUEST_TIMEOUT * 1000);

            //  If we got a reply, process it
            if (items [0].revents & ZMQ_POLLIN) {
                //  We got a reply from the server, must match sequence
                char *reply = s_recv (client);
                if (atoi (reply) == sequence) {
                    printf ("I: server replied OK (%s)\n", reply);
                    retries_left = REQUEST_RETRIES;
                    expect_reply = 0;
                }
                else
                    printf ("E: malformed reply from server: %s\n", reply);

                free (reply);
            }
            else
            if (--retries_left == 0) {
                printf ("E: server seems to be offline, abandoning\n");
                break;
            }
            else {
                printf ("W: no response from server, retrying...\n");
                //  Old socket will be confused; close it and open a new one
                zmq_close (client);
                client = s_client_socket (context);
                //  Send request again, on new socket
                s_send (client, request);
            }
        }
    }
    zmq_close (client);
    zmq_term (context);
    return 0;
}
