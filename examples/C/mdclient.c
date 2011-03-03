//
//  Majordomo Protocol client example
//  Uses the mdcli API to hide all MDP aspects
//
//  Lets us 'build mdclient' and 'build all'
#include "mdcliapi.c"

int main (void)
{
    mdcli_t *session = mdcli_new ("tcp://localhost:5555", 0);

    int count;
    for (count = 0; count < 100000; count++) {
        zmsg_t *request = zmsg_new ("Hello world");
        zmsg_t *reply = mdcli_send (session, "echo", request);
        zmsg_destroy (&request);
        if (reply)
            zmsg_destroy (&reply);
        else
            break;              //  Interrupted by Ctrl-C
    }
    printf ("%d requests/replies processed\n", count);
    mdcli_destroy (&session);
    return 0;
}
