//
//  Titanic service
//
//  Implements these MDP services:
//  titanic-request
//      - store a request message, return a UUID for the request.
//  titanic-reply
//      - fetch a reply, if available, for a given request UUID.
//  titanic-close
//      - confirm that a reply has been stored and processed.
//

#include "mdwrkapi.c"
#include <uuid/uuid.h>

//  Return a new UUID as a printable character string
//  Caller must free returned string when finished with it

static char *
s_get_new_uuid (void)
{
    char hex_char [] = "0123456789ABCDEF";
    char *uuidstr = calloc (1, sizeof (uuid_t) * 2 + 1);
    uuid_t uuid;
    uuid_generate (uuid);
    int byte_nbr;
    for (byte_nbr = 0; byte_nbr < sizeof (uuid_t); byte_nbr++) {
        uuidstr [byte_nbr * 2 + 0] = hex_char [uuid [byte_nbr] >> 4];
        uuidstr [byte_nbr * 2 + 1] = hex_char [uuid [byte_nbr] & 15];
    }
    return (uuidstr);
}


static void *
titanic_request (void *args)
{
    mdwrk_t *session = mdwrk_new ("tcp://localhost:5555", "titanic-request", 1);
    zmsg_t *reply = NULL;
    while (1) {
        zmsg_t *request = mdwrk_recv (session, &reply);
        if (!request)
            break;
        char *uuid = s_get_new_uuid ();

        //- create <uuid>.req disk file with message
        //- zmsg_save/zmsg_load

        reply = zmsg_new (uuid);
        free (uuid);
        zmsg_destroy (&request);
    }
    mdwrk_destroy (&session);
    return 0;
}

static void *
titanic_reply (void *context)
{
    mdwrk_t *session = mdwrk_new ("tcp://localhost:5555", "titanic-reply", 1);
    zmsg_t *reply = NULL;
    while (1) {
        zmsg_t *request = mdwrk_recv (session, &reply);
        if (!request)
            break;
        //- check if <uuid>.rep file exists
        //- zmsg_load() into message
        //- return to client
    }
    mdwrk_destroy (&session);
    return 0;
}

static void *
titanic_close (void *context)
{
    mdwrk_t *session = mdwrk_new ("tcp://localhost:5555", "titanic-close", 1);
    zmsg_t *reply = NULL;
    while (1) {
        zmsg_t *request = mdwrk_recv (session, &reply);
        if (!request)
            break;
        //- remove <uuid>.req and <uuid.rep> if any
    }
    mdwrk_destroy (&session);
    return 0;
}


int main (void)
{
    s_version_assert (2, 1);
    pthread_t thread;
    pthread_create (&thread, NULL, titanic_request, NULL);
    pthread_join (thread, NULL);

    // Dispatcher thread
    //  - loop over all messages
    //  - try to dispatch them, see what comes back
    //  - very very very simple minded
    //  - list the optimizations we can make
    return 0;
}
