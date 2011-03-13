//
//  Freelance client - Model 1
//  Uses REQ socket to query one or more services
//
#include "zmsg.h"

#define REQUEST_TIMEOUT     1000
#define MAX_RETRIES         3       //  Before we abandon


static zmsg_t *
s_try_request (void *context, char *endpoint, zmsg_t *request) 
{
    zmsg_t *reply = NULL;
    
    printf ("I: trying echo service at %s...\n", endpoint);
    void *client = zmq_socket (context, ZMQ_REQ);
    int rc = zmq_connect (client, endpoint);
    assert (rc == 0);

    //  Send request, wait safely for reply
    zmsg_t *msg = zmsg_dup (request);
    zmsg_send (&msg, client);
    zmq_pollitem_t items [] = { { client, 0, ZMQ_POLLIN, 0 } };
    zmq_poll (items, 1, REQUEST_TIMEOUT * 1000);
    if (items [0].revents & ZMQ_POLLIN)
        reply = zmsg_recv (client);

    //  Close socket in any case, we're done with it now
    int zero = 0;
    zmq_setsockopt (client, ZMQ_LINGER, &zero, sizeof (zero));
    zmq_close (client);
    return reply;
}


int main (int argc, char *argv [])
{
    void *context = zmq_init (1);
    zmsg_t *request = zmsg_new ("Hello world");
    zmsg_t *reply = NULL;
    
    int endpoints = argc - 1;
    if (endpoints == 0) {
        printf ("I: syntax: %s <endpoint> ...\n", argv [0]);
        exit (EXIT_SUCCESS);
    }
    else
    if (endpoints == 1) {
        //  For one endpoint, we retry N times
        int retries;
        for (retries = 0; retries < MAX_RETRIES; retries++) {
            char *endpoint = argv [1];
            reply = s_try_request (context, endpoint, request);
            if (reply)
                break;          //  Successful
            printf ("W: no response from %s, retrying...\n", endpoint);
        }
    }
    else {
        //  For multiple endpoints, try each at most once
        int endpoint_nbr;
        for (endpoint_nbr = 0; endpoint_nbr < endpoints; endpoint_nbr++) {
            char *endpoint = argv [endpoint_nbr + 1];
            reply = s_try_request (context, endpoint, request);
            if (reply)
                break;          //  Successful
            printf ("W: no response from %s\n", endpoint);
        }
    }
    if (reply) 
        printf ("Service is running, OK ('%s')\n", zmsg_body (reply));
    
    zmsg_destroy (&request);
    zmsg_destroy (&reply);
    zmq_term (context);
    return 0;
}

