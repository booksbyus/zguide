//
//  Majordomo client
//
#include "mdcliapi.c"

int main (void)
{
    mdcli_t *client = mdcli_new ("tcp://localhost:5555");

    zmsg_t *request = zmsg_new ();
    zmsg_append (request, "Hello world");
    zmsg_t *reply = mdcli_send (client, "echo", request);
    zmsg_dump (reply);
    zmsg_destroy (&reply);

    mdcli_destroy (&client);

    return 0;
}