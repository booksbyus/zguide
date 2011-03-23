//
//  Freelance client - Model 3
//  Uses flcliapi class to encapsulate Freelance pattern
//
#include "flcliapi.c"

int main (void)
{
    //  Create new freelance client object
    flcliapi_t *client = flcliapi_new ();

    //  Connect to several endpoints
    flcliapi_connect (client, "tcp://localhost:5555");
    flcliapi_connect (client, "tcp://localhost:5556");
    flcliapi_connect (client, "tcp://localhost:5557");

    //  Send a bunch of name resolution 'requests', measure time
    int requests = 10000;
    uint64_t start = s_clock ();
    while (requests--) {
        zmsg_t *request = zmsg_new ("random name");
        zmsg_t *reply = flcliapi_request (client, &request);
        if (!reply) {
            printf ("E: name service not available, aborting\n");
            break;
        }
        zmsg_destroy (&reply);
    }
    printf ("Average round trip cost: %d usec\n", 
        (int) (s_clock () - start) / 10);
    flcliapi_destroy (&client);
    return 0;
}
