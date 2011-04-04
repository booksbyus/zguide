//
//  Freelance server - Model 3
//  Uses an ROUTER/ROUTER socket but just one thread
//
#include "zapi.h"

int main (int argc, char *argv [])
{
    int verbose = (argc > 1 && streq (argv [1], "-v"));

    zctx_t *ctx = zctx_new ();

    //  Prepare server socket with predictable identity
    char *bind_endpoint = "tcp://*:5555";
    char *connect_endpoint = "tcp://localhost:5555";
    void *server = zctx_socket_new (ctx, ZMQ_ROUTER);
    zmq_setsockopt (server,
        ZMQ_IDENTITY, connect_endpoint, strlen (connect_endpoint));
    zmq_bind (server, bind_endpoint);
    printf ("I: service is ready at %s\n", bind_endpoint);

    while (!zctx_interrupted) {
        zmsg_t *request = zmsg_recv (server);
        zmsg_t *reply = NULL;
        if (verbose && request)
            zmsg_dump (request);
        if (!request)
            break;          //  Interrupted

        //  Frame 0: identity of client
        //  Frame 1: PING, or client control frame
        //  Frame 2: request body
        char *address = zmsg_pop (request);
        if (zmsg_parts (request) == 1
        && streq (zmsg_body (request), "PING"))
            reply = zmsg_new ("PONG");
        else
        if (zmsg_parts (request) > 1) {
            reply = request;
            request = NULL;
            zmsg_body_set (reply, "OK");
        }
        zmsg_destroy (&request);
        zmsg_pushstr (reply, address);
        if (verbose && reply)
            zmsg_dump (reply);
        zmsg_send (&reply, server);
        free (address);
    }
    if (zctx_interrupted)
        printf ("W: interrupted\n");

    zctx_destroy (&ctx);
    return 0;
}
