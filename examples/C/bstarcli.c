//
//  Binary Star client
//
#include "zhelpers.h"

#define REQUEST_TIMEOUT     1000    //  msecs
#define SETTLE_DELAY        2000    //  Before failing over

//  Helper function that returns a connected socket
//
static void *
s_client_socket (void *context, char *server)
{
    printf ("I: connecting to server at %s...\n", server);
    void *client = zmq_socket (context, ZMQ_REQ);
    zmq_connect (client, server);
    int linger = 0;
    zmq_setsockopt (client, ZMQ_LINGER, &linger, sizeof (linger));
    return client;
}

int main (void)
{
    char *server [] = { "tcp://localhost:5001", "tcp://localhost:5002" };
    uint server_nbr = 0;
    void *context = zmq_init (1);
    void *client = s_client_socket (context, server [server_nbr]);

    int sequence = 0;
    while (!s_interrupted) {
        //  We send a request, then we work to get a reply
        char request [10];
        sprintf (request, "%d", ++sequence);
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
                    expect_reply = 0;
                    sleep (1);  //  One request per second
                }
                else {
                    printf ("E: malformed reply from server: %s\n", 
                        reply);
                }
                free (reply);
            }
            else {
                printf ("W: no response from server, failing over\n");
                //  Old socket is confused; close it and open a new one
                zmq_close (client);
                server_nbr = (server_nbr + 1) % 2;
                s_sleep (SETTLE_DELAY);
                client = s_client_socket (context, server [server_nbr]);
                //  Send request again, on new socket
                s_send (client, request);
            }
        }
    }
    zmq_close (client);
    zmq_term (context);
    return 0;
}
