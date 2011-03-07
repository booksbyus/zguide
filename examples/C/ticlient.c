//
//  Titanic client example
//
//  Lets us 'build mdclient' and 'build all'
#include "mdcliapi.c"

int main (void)
{
    mdcli_t *session = mdcli_new ("tcp://localhost:5555", 1);

    zmsg_t *request = zmsg_new ("Hello world");
    zmsg_push (request, "echo");
    zmsg_t *reply = mdcli_send (session, "titanic-request", &request);
    zmsg_destroy (&reply);

    mdcli_destroy (&session);
    return 0;
}
