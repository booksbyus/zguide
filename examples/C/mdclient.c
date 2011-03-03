//
//  Majordomo Protocol client example
//  Uses the mdcli API to hide all MDP aspects
//
#include "mdcliapi.class"

int main (void)
{
    mdcli_t *session = mdcli_new ("tcp://localhost:5555", 0);

    int count;
    for (count = 0; count < 100000; count++) {
        zmsg_t *request = zmsg_new ();
        zmsg_append (request, "Hello world");
        zmsg_t *reply = mdcli_send (session, "echo", request);
        zmsg_destroy (&reply);
        zmsg_destroy (&request);
    }
    puts ("100K requests/replies processed");
    mdcli_destroy (&session);
    return 0;
}