//
//  Majordomo Protocol client example
//  Uses the mdcli API to hide all MDP aspects
//
#include "mdcliapi.c"

int main (void)
{
    mdcli_t *session = mdcli_new ("tcp://localhost:5555", 1);

    zmsg_t *request = zmsg_new ();
    zmsg_append (request, "Hello world");
    zmsg_t *reply = mdcli_send (session, "echo", request);
    if (reply) {
        zmsg_dump (reply);
        zmsg_destroy (&reply);
    }
    mdcli_destroy (&session);
    return 0;
}