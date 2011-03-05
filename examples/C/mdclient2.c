//
//  Majordomo Protocol client example - asynchronous
//  Uses the mdcli API to hide all MDP aspects
//
//  Lets us 'build mdclient' and 'build all'
#include "mdcliapi2.c"

int main (void)
{
    mdcli_t *session = mdcli_new ("tcp://localhost:5555", 0);

    int count;
    for (count = 0; count < 100000; count++) {
        zmsg_t *request = zmsg_new ("Hello world");
        mdcli_send (session, "echo", &request);
    }
    for (count = 0; count < 100000; count++) {
        zmsg_t *reply = mdcli_recv (session);
        if (reply)
            zmsg_destroy (&reply);
        else
            break;              //  Interrupted by Ctrl-C
    }
    printf ("%d replies received\n", count);
    mdcli_destroy (&session);
    return 0;
}
