//
//  Majordomo Protocol worker example
//  Uses the mdwrk API to hide all MDP aspects
//
//  Lets us 'build mdworker' and 'build all'
#include "mdwrkapi.c"

int main (void)
{
    mdwrk_t *session = mdwrk_new ("tcp://localhost:5555", "echo", 1);

    zmsg_t *reply = NULL;
    while (1) {
        zmsg_t *request = mdwrk_recv (session, &reply);
        if (request == NULL)
            break;              //  Worker was interrupted
        reply = request;        //  Echo is complex... :-)
    }
    mdwrk_destroy (&session);
    return 0;
}
