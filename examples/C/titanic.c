//
//  Titanic service
//
//  Implements server side of http://rfc.zeromq.org/spec:9

//  Lets us build this source without creating a library
#include "mdwrkapi.c"
#include "mdcliapi.c"

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
    return uuidstr;
}

//  Returns freshly allocated request filename for given UUID

#define TITANIC_DIR ".titanic"

static char *
s_request_filename (char *uuid) {
    char *filename = malloc (256);
    snprintf (filename, 256, TITANIC_DIR "/%s.req", uuid);
    return filename;
}

//  Returns freshly allocated reply filename for given UUID

static char *
s_reply_filename (char *uuid) {
    char *filename = malloc (256);
    snprintf (filename, 256, TITANIC_DIR "/%s.rep", uuid);
    return filename;
}


//  ---------------------------------------------------------------------
//  Titanic request service

static void *
titanic_request (void *context)
{
    //  We send new requests through to inproc://queue
    void *queue = zmq_socket (context, ZMQ_PAIR);
    zmq_connect (queue, "inproc://queue");

    mdwrk_t *worker = mdwrk_new (
        "tcp://localhost:5555", "titanic.request", 0);
    zmsg_t *reply = NULL;

    while (1) {
        //  Get next request from broker
        zmsg_t *request = mdwrk_recv (worker, &reply);
        if (!request)
            break;      //  Interrupted, exit

        //  Ensure message directory exists
        file_mkdir (TITANIC_DIR);

        //  Generate UUID and save message to disk
        char *uuid = s_generate_uuid ();
        char *filename = s_request_filename (uuid);
        FILE *file = fopen (filename, "w");
        assert (file);
        zmsg_save (&request, file);
        fclose (file);
        free (filename);

        //  Send UUID through to message queue
        reply = zmsg_new (uuid);
        zmsg_send (&reply, queue);

        //  Now send UUID back to client
        reply = zmsg_new (uuid);
        zmsg_push (reply, "200 OK");
        free (uuid);
    }
    mdwrk_destroy (&worker);
    return 0;
}


//  ---------------------------------------------------------------------
//  Titanic reply service

static void *
titanic_reply (void *context)
{
    mdwrk_t *worker = mdwrk_new (
        "tcp://localhost:5555", "titanic.reply", 0);
    zmsg_t *reply = NULL;

    while (1) {
        zmsg_t *request = mdwrk_recv (worker, &reply);
        if (!request)
            break;      //  Interrupted, exit

        char *filename = s_reply_filename (zmsg_body (request));
        if (file_exists (filename)) {
            FILE *file = fopen (filename, "r");
            assert (file);
            reply = zmsg_load (file);
            zmsg_push (reply, "200 OK");
            fclose (file);
        }
        else {
            char *filename = s_request_filename (zmsg_body (request));
            if (file_exists (filename))
                reply = zmsg_new ("300 PENDING");
            else
                reply = zmsg_new ("400 UNKNOWN");
            free (filename);
        }
        zmsg_destroy (&request);
        free (filename);
    }
    mdwrk_destroy (&worker);
    return 0;
}


//  ---------------------------------------------------------------------
//  Titanic close service

static void *
titanic_close (void *context)
{
    mdwrk_t *worker = mdwrk_new (
        "tcp://localhost:5555", "titanic.close", 0);
    zmsg_t *reply = NULL;

    while (1) {
        zmsg_t *request = mdwrk_recv (worker, &reply);
        if (!request)
            break;      //  Interrupted, exit

        char *filename;
        filename = s_request_filename (zmsg_body (request));
        file_delete (filename);
        free (filename);

        filename = s_reply_filename (zmsg_body (request));
        file_delete (filename);
        free (filename);

        zmsg_destroy (&request);
        reply = zmsg_new ("200 OK");
    }
    mdwrk_destroy (&worker);
    return 0;
}

//  Attempt to process a single request, return 1 if successful

static int
s_service_success (mdcli_t *client, char *uuid)
{
    //  Load request message, service will be first frame
    char *filename = s_request_filename (uuid);
    FILE *file = fopen (filename, "r");
    free (filename);

    //  If the client already closed request, treat as successful
    if (!file)
        return 1;

    zmsg_t *request = zmsg_load (file);
    char *service = zmsg_pop (request);
    fclose (file);

    //  Use MMI protocol to check if service is available
    zmsg_t *mmi_request = zmsg_new (service);
    zmsg_t *mmi_reply = mdcli_send (client, "mmi.service", &mmi_request);
    int service_ok = (mmi_reply && atoi (zmsg_body (mmi_reply)) == 200);
    zmsg_destroy (&mmi_reply);

    if (service_ok) {
        zmsg_t *reply = mdcli_send (client, service, &request);
        if (reply) {
            filename = s_reply_filename (uuid);
            FILE *file = fopen (filename, "w");
            assert (file);
            zmsg_save (&reply, file);
            fclose (file);
            free (filename);
            return 1;
        }
        zmsg_destroy (&reply);
    }
    else
        zmsg_destroy (&request);

    free (service);
    return 0;
}


int main (int argc, char *argv [])
{
    int verbose = (argc > 1 && streq (argv [1], "-v"));
    s_version_assert (2, 1);
    void *context = zmq_init (1);

    //  We expect new requests through on inproc://queue
    //  This is to avoid multiple writers on the queue file
    void *queue = zmq_socket (context, ZMQ_PAIR);
    zmq_bind (queue, "inproc://queue");

    //  Create MDP client session with short timeout
    mdcli_t *client = mdcli_new ("tcp://localhost:5555", verbose);
    mdcli_set_timeout (client, 1000);  //  1 sec
    mdcli_set_retries (client, 1);     //  only 1 retry

    pthread_t thread;
    pthread_create (&thread, NULL, titanic_request, context);
    pthread_create (&thread, NULL, titanic_reply, NULL);
    pthread_create (&thread, NULL, titanic_close, NULL);

    //  Main dispatcher loop
    while (!s_interrupted) {
        //  We'll dispatch once per second, if there's no activity
        zmq_pollitem_t items [] = { { queue, 0, ZMQ_POLLIN, 0 } };
        zmq_poll (items, 1, 1000 * 1000);
        if (items [0].revents & ZMQ_POLLIN) {
            //  Ensure message directory exists
            file_mkdir (TITANIC_DIR);

            //  Append UUID to queue, prefixed with '-' for pending
            zmsg_t *msg = zmsg_recv (queue);
            FILE *file = fopen (TITANIC_DIR "/queue", "a");
            fprintf (file, "-%s\n", zmsg_body (msg));
            fclose (file);
            zmsg_destroy (&msg);
        }
        //  Brute-force dispatcher
        //
        char entry [] = "?.......:.......:.......:.......:";
        FILE *file = fopen (TITANIC_DIR "/queue", "r+");
        while (file && fread (entry, 33, 1, file) == 1) {
            //  UUID is prefixed with '-' if still waiting
            if (entry [0] == '-') {
                if (verbose)
                    printf ("I: processing request %s\n", entry + 1);
                if (s_service_success (client, entry + 1)) {
                    //  Mark queue entry as processed
                    fseek (file, -33, SEEK_CUR);
                    fwrite ("+", 1, 1, file);
                    fseek (file, 32, SEEK_CUR);
                }
            }
            //  Skip end of line, LF or CRLF
            if (fgetc (file) == '\r')
                fgetc (file);
            if (s_interrupted)
                break;
        }
        if (file)
            fclose (file);
    }
    mdcli_destroy (&client);
    return 0;
}
