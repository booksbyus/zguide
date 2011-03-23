//
//  Freelance client - Model 2
//  Uses DEALER socket to blast one or more services
//
#include "zmsg.h"

//  If not a single service replies within this time, give up
#define GLOBAL_TIMEOUT 2500

//  We design our client API as a class

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _flclient_t flclient_t;

flclient_t *flclient_new     (void);
void        flclient_destroy (flclient_t **self_p);
void        flclient_connect (flclient_t *self, char *endpoint);
zmsg_t *    flclient_request (flclient_t *self, zmsg_t **request_p);

#ifdef __cplusplus
}
#endif


int main (int argc, char *argv [])
{
    if (argc == 1) {
        printf ("I: syntax: %s <endpoint> ...\n", argv [0]);
        exit (EXIT_SUCCESS);
    }
    //  Create new freelance client object
    flclient_t *client = flclient_new ();
    
    //  Connect to each endpoint
    int argn;
    for (argn = 1; argn < argc; argn++)
        flclient_connect (client, argv [argn]);
    
    //  Send a bunch of name resolution 'requests', measure time
    int requests = 10000;
    uint64_t start = s_clock ();
    while (requests--) {
        zmsg_t *request = zmsg_new ("random name");
        zmsg_t *reply = flclient_request (client, &request);
        if (!reply) {
            printf ("E: name service not available, aborting\n");
            exit (EXIT_FAILURE);
        }
        zmsg_destroy (&reply);
    }
    printf ("Average round trip cost: %d usec\n", 
        (int) (s_clock () - start) / 10);
    
    flclient_destroy (&client);
    return 0;
}

    

//  --------------------------------------------------------------------
//  Structure of our class

struct _flclient_t {
    void *context;      //  Our 0MQ context
    void *socket;       //  DEALER socket talking to servers
    size_t servers;     //  How many servers we have connected to
    uint sequence;      //  Number of requests ever sent
};


//  --------------------------------------------------------------------
//  Constructor

flclient_t *
flclient_new (void)
{
    flclient_t
        *self;

    self = (flclient_t *) calloc (1, sizeof (flclient_t));
    self->context = zmq_init (1);
    self->socket = zmq_socket (self->context, ZMQ_XREQ);
    return self;
}

//  --------------------------------------------------------------------
//  Destructor

void
flclient_destroy (flclient_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        flclient_t *self = *self_p;

        int zero = 0;
        zmq_setsockopt (self->socket, ZMQ_LINGER, &zero, sizeof (zero));
        zmq_close (self->socket);
        zmq_term (self->context);

        //  Free object structure
        free (self);
        *self_p = NULL;
    }
}

//  --------------------------------------------------------------------
//  Connect to new server endpoint

void
flclient_connect (flclient_t *self, char *endpoint)
{
    assert (self);
    int rc = zmq_connect (self->socket, endpoint);
    assert (rc == 0);
    self->servers++;
}

//  --------------------------------------------------------------------
//  Send request, get reply
//  Destroys request after sending

zmsg_t *
flclient_request (flclient_t *self, zmsg_t **request_p)
{
    assert (self);
    assert (*request_p);
    zmsg_t *request = *request_p;
    
    //  Prefix request with sequence number and empty envelope
    char sequence_text [10];
    sprintf (sequence_text, "%u", ++self->sequence);
    zmsg_push (request, sequence_text);
    zmsg_push (request, "");
    
    //  Blast the request to all connected servers
    int server;
    for (server = 0; server < self->servers; server++) {
        zmsg_t *msg = zmsg_dup (request);
        zmsg_send (&msg, self->socket);
    }
    //  Wait for a matching reply to arrive from anywhere
    //  Since we can poll several times, calculate each one
    zmsg_t *reply = NULL;
    uint64_t endtime = s_clock () + GLOBAL_TIMEOUT;
    while (s_clock () < endtime) {
        zmq_pollitem_t items [] = { { self->socket, 0, ZMQ_POLLIN, 0 } };
        zmq_poll (items, 1, (endtime - s_clock ()) * 1000);
        if (items [0].revents & ZMQ_POLLIN) {
            reply = zmsg_recv (self->socket);
            assert (zmsg_parts (reply) == 3);
            free (zmsg_pop (reply));
            if (atoi (zmsg_address (reply)) == self->sequence)
                break;
            zmsg_destroy (&reply);
        }
    }
    zmsg_destroy (request_p);
    return reply;
}
