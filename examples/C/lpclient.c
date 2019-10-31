#include <czmq.h>
#define REQUEST_TIMEOUT 2500 // msecs, (>1000!)
#define REQUEST_RETRIES 3 // Before we abandon
#define SERVER_ENDPOINT "tcp://localhost:5555"

int main()
{
    zsock_t *client = zsock_new_req(SERVER_ENDPOINT);
    printf("I: Connecting to server...\n");
    assert(client);

    int sequence = 0;
    int retries_left = REQUEST_RETRIES;
    printf("Entering while loop...\n");
    while(retries_left) // interrupt needs to be handled
    {
        // We send a request, then we get a reply
        char request[10];
        sprintf(request, "%d", ++sequence);
        zstr_send(client, request);
        int expect_reply = 1;
        while(expect_reply)
        {
            printf("Expecting reply....\n");
            zmq_pollitem_t items [] = {{zsock_resolve(client), 0, ZMQ_POLLIN, 0}};
            printf("After polling\n");
            int rc = zmq_poll(items, 1, REQUEST_TIMEOUT * ZMQ_POLL_MSEC);
            printf("Polling Done.. \n");
            if (rc == -1)
                break; // Interrupted
            
            // Here we process a server reply and exit our loop if the
            // reply is valid. If we didn't get a reply we close the
            // client socket, open it again and resend the request. We
            // try a number times before finally abandoning:

            if (items[0].revents & ZMQ_POLLIN)
            {
                // We got a reply from the server, must match sequence
                char *reply = zstr_recv(client);
                if(!reply)
                    break; // interrupted
                if (atoi(reply) == sequence)
                {
                    printf("I: server replied OK (%s)\n", reply);
                    retries_left=REQUEST_RETRIES;
                    expect_reply = 0;
                }
                else
                {
                    printf("E: malformed reply from server: %s\n", reply);
                }
                free(reply);
            }
            else 
            {
                if(--retries_left == 0)
                {
                    printf("E: Server seems to be offline, abandoning\n");
                    break;
                }
                else
                {
                    printf("W: no response from server, retrying...\n");
                    zsock_destroy(&client);
                    printf("I: reconnecting to server...\n");
                    client = zsock_new_req(SERVER_ENDPOINT);
                    zstr_send(client, request);
                }
            }
        }
        zsock_destroy(&client);
        return 0;
    }
}