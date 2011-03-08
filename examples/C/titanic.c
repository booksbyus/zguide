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
#include "zfile.h"
#include <uuid/uuid.h>

//  Return a new UUID as a printable character string
//  Caller must free returned string when finished with it

static char *
s_generate_uuid (void)
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

//  Returns freshly allocated request filename for given UUID

#define TITANIC_DIR ".titanic"

static char *
s_request_filename (char *uuid) {
    char *filename = malloc (256);
    snprintf (filename, 256, TITANIC_DIR "/%s.req", uuid);
    return (filename);
}

//  Returns freshly allocated reply filename for given UUID

static char *
s_reply_filename (char *uuid) {
    char *filename = malloc (256);
    snprintf (filename, 256, TITANIC_DIR "/%s.rep", uuid);
    return (filename);
}


//  -----------------------------------------------------------------------
//  Titanic request service

static void *
titanic_request (void *args)
{
    mdwrk_t *session = mdwrk_new (
        "tcp://localhost:5555", "titanic-request", 1);
    zmsg_t *reply = NULL;
    while (1) {
        //  Get next request from broker
        zmsg_t *request = mdwrk_recv (session, &reply);
        if (!request)
            break;

        //  Generate UUID and save message to disk
        char *uuid = s_generate_uuid ();
        char *filename = s_request_filename (uuid);
        FILE *file = fopen (filename, "w");
        assert (file);
        zmsg_save (&request, file);
        fclose (file);
        free (filename);

        //  Reply to client with generated UUID
        reply = zmsg_new (uuid);
        free (uuid);
    }
    mdwrk_destroy (&session);
    return 0;
}


//  -----------------------------------------------------------------------
//  Titanic reply service

static void *
titanic_reply (void *context)
{
    mdwrk_t *session = mdwrk_new (
        "tcp://localhost:5555", "titanic-reply", 1);
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


//  -----------------------------------------------------------------------
//  Titanic close service

static void *
titanic_close (void *context)
{
    mdwrk_t *session = mdwrk_new (
        "tcp://localhost:5555", "titanic-close", 1);
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

    //  Prepare working directory
    file_mkdir (TITANIC_DIR);

    pthread_t thread;
    pthread_create (&thread, NULL, titanic_request, NULL);
//    pthread_create (&thread, NULL, titanic_reply, NULL);
 //   pthread_create (&thread, NULL, titanic_close, NULL);
    
    pthread_join (thread, NULL);

    // Dispatcher thread
    //  - loop over all messages
    //  - try to dispatch them, see what comes back
    //  - very very very simple minded
    //  - list the optimizations we can make
    return 0;
}
