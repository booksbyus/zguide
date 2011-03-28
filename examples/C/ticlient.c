//
//  Titanic client example
//  Implements client side of http://rfc.zeromq.org/spec:9

//  Lets us 'build mdclient' and 'build all'
#include "mdcliapi.c"

//  Calls a TSP service
//  Returns reponse if successful (status code 200), else NULL
//
static zmsg_t *
s_service_call (mdcli_t *session, char *service, zmsg_t **request_p)
{
    zmsg_t *reply = mdcli_send (session, service, request_p);
    if (reply) {
        char *status_text = zmsg_pop (reply);
        int status_code = atoi (status_text);
        free (status_text);

        if (status_code == 200)
            return reply;
        else
        if (status_code == 400) {
            printf ("E: client fatal error, aborting\n");
            exit (EXIT_FAILURE);
        }
        else
        if (status_code == 500) {
            printf ("E: server fatal error, aborting\n");
            exit (EXIT_FAILURE);
        }
        zmsg_destroy (&reply);
    }
    else
        exit (EXIT_SUCCESS);    //  Interrupted or failed

    return NULL;        //  Didn't succeed, don't care why not
}

int main (int argc, char *argv [])
{
    int verbose = (argc > 1 && streq (argv [1], "-v"));
    mdcli_t *session = mdcli_new ("tcp://localhost:5555", verbose);

    //  1. Send 'echo' request to Titanic
    zmsg_t *request = zmsg_new ("Hello world");
    zmsg_push (request, "echo");
    zmsg_t *reply = s_service_call (
        session, "titanic.request", &request);
    char *uuid = NULL;
    if (reply) {
        uuid = zmsg_pop (reply);
        zmsg_destroy (&reply);
        printf ("I: request UUID: %s\n", uuid);
    }

    //  2. Wait until we get a reply
    while (!s_interrupted) {
        s_sleep (100);
        request = zmsg_new (uuid);
        zmsg_t *reply = s_service_call (
            session, "titanic.reply", &request);
        if (reply) {
            printf ("Reply: %s\n", zmsg_body (reply));
            zmsg_destroy (&reply);

            //  3. Close request
            request = zmsg_new (uuid);
            reply = s_service_call (session, "titanic.close", &request);
            zmsg_destroy (&reply);
            break;
        }
        else {
            printf ("I: no reply yet, trying again...\n");
            s_sleep (5000);     //  Try again in 5 seconds
        }
    }
    mdcli_destroy (&session);
    return 0;
}
