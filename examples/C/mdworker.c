//
//  Majordomo Protocol worker example
//  Uses the mdwrk API to hide all MDP aspects
//
#include "mdwrkapi.class"

int main (void)
{
    mdwrk_t *session = mdwrk_new ("tcp://localhost:5555", "echo", 0);

    zmsg_t *reply = NULL;
    while (1) {
        zmsg_t *request = mdwrk_recv (session, reply);
        // todo free reply
        zmsg_destroy (&reply);
        if (request == NULL)
            break;
//        s_console ("I: Worker application received request");
 //       zmsg_dump (request);

        //  Echo is complex... :-)
        reply = request;
    }
    mdwrk_destroy (&session);
    return 0;
}
