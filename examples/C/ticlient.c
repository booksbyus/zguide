//
//  Titanic client example
//
//  Lets us 'build mdclient' and 'build all'
#include "mdcliapi.c"

int main (int argc, char *argv [])
{
    int verbose = (argc > 1 && strcmp (argv [1], "-v") == 0);
    mdcli_t *session = mdcli_new ("tcp://localhost:5555", verbose);

    //  1. Send request to Titanic
    zmsg_t *request = zmsg_new ("Hello world");
    zmsg_push (request, "echo");
    zmsg_t *reply = mdcli_send (session, "titanic-request", &request);
    if (!reply) {
        printf ("E: can't reach titanic-request service\n");
        return 0;
    }
    char *uuid = zmsg_pop (reply);
    zmsg_destroy (&reply);
    printf ("I: request UUID: %s\n", uuid);

    //  2. Wait until we get a reply
    while (1) {
        s_sleep (100);
        request = zmsg_new (uuid);
        reply = mdcli_send (session, "titanic-reply", &request);
        if (!reply) {
            printf ("E: can't reach titanic-reply service\n");
            return 0;
        }
        char *status = zmsg_pop (reply);
        if (atoi (status) == 200) {
            printf ("Reply: %s\n", zmsg_body (reply));
            zmsg_destroy (&reply);

            //  3. Close request
            request = zmsg_new (uuid);
            reply = mdcli_send (session, "titanic-close", &request);
            if (!reply)
                printf ("E: can't reach titanic-close service\n");
            break;
        }
        else {
            printf ("I: no reply yet, trying again...\n");
            s_sleep (5000);     //  Try again in 5 seconds
        }
    }
    zmsg_destroy (&reply);
    mdcli_destroy (&session);
    return 0;
}
