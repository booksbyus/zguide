//
//  Freelance client - Model 3
//  Uses flclient class to encapsulate Freelance pattern
//
#include "flcliapi.c"

int main (void)
{
    //  Create new freelance client object
    flclient_t *client = flclient_new ();

    //  Connect to several endpoints
    flclient_connect (client, "tcp://localhost:5555");
    flclient_connect (client, "tcp://localhost:5556");
    flclient_connect (client, "tcp://localhost:5557");

    //  Send a bunch of name resolution 'requests', measure time
    int requests = 10000;
    uint64_t start = s_clock ();
    while (requests--) {
        zmsg_t *request = zmsg_new ("random name");
        zmsg_t *reply = flclient_request (client, &request);
        if (!reply) {
            printf ("E: name service not available, aborting\n");
            break;
        }
        zmsg_destroy (&reply);
    }
    printf ("Average round trip cost: %d usec\n", 
        (int) (s_clock () - start) / 10);
    flclient_destroy (&client);
    return 0;
}
