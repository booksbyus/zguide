//
//  Majordomo Protocol Client API
//  Implements the MDP/Client spec at http://rfc.zeromq.org/spec:7
//
#include "zmsg.class"
#include "mdp.h"

//  Reliability parameters
#define REQUEST_TIMEOUT     2500    //  msecs, (> 1000!)
#define REQUEST_RETRIES     3       //  Before we abandon

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _mdcli_t mdcli_t;

mdcli_t *mdcli_new     (char *broker);
void     mdcli_destroy (mdcli_t **self_p);
zmsg_t  *mdcli_send    (mdcli_t *self, char *service, zmsg_t *request);

#ifdef __cplusplus
}
#endif

//  Structure of our class
//  We access these properties only via class methods

struct _mdcli_t {
    char *broker;
    void *context;
    void *client;               //  Socket to broker
};


//  --------------------------------------------------------------------------
//  Connect or reconnect to broker

void s_connect_to_broker (mdcli_t *self)
{
    if (self->client)
        zmq_close (self->client);
    self->client = zmq_socket (self->context, ZMQ_REQ);
    int linger = 0;
    zmq_setsockopt (self->client, ZMQ_LINGER, &linger, sizeof (linger));
    zmq_connect (self->client, self->broker);
}


//  --------------------------------------------------------------------------
//  Constructor

mdcli_t *
mdcli_new (char *broker)
{
    mdcli_t
        *self;

    assert (broker);
    s_version_assert (2, 1);
    self = malloc (sizeof (mdcli_t));
    memset (self, 0, sizeof (mdcli_t));

    self->broker = strdup (broker);
    self->context = zmq_init (1);
    s_connect_to_broker (self);
    return (self);
}


//  --------------------------------------------------------------------------
//  Destructor

void
mdcli_destroy (mdcli_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        mdcli_t *self = *self_p;
        zmq_close (self->client);
        zmq_term (self->context);
        free (self->broker);
        free (self);
        *self_p = NULL;
    }
}


//  --------------------------------------------------------------------------
//  Send request to broker and get reply by hook or crook
//  Returns the reply message or NULL if there was no reply.

zmsg_t *
mdcli_send (mdcli_t *self, char *service, zmsg_t *request)
{
    int retries_left = REQUEST_RETRIES;
    while (retries_left) {
        //  Prefix request with protocol frames
        //  Frame 1: "MDPCxy" (six bytes, MDP/Client x.y)
        //  Frame 2: Service name (printable string)
        zmsg_t *msg = zmsg_dup (request);
        zmsg_push (msg, service);
        zmsg_push (msg, MDPC_CLIENT);
        zmsg_send (&msg, self->client);

        while (1) {
            //  Poll socket for a reply, with timeout
            zmq_pollitem_t items [] = { { self->client, 0, ZMQ_POLLIN, 0 } };
            zmq_poll (items, 1, REQUEST_TIMEOUT * 1000);

            //  If we got a reply, process it
            if (items [0].revents & ZMQ_POLLIN) {
                zmsg_t *msg = zmsg_recv (self->client);

                //  Don't try to handle errors, just assert noisily
                assert (zmsg_parts (msg) >= 3);

                char *header = zmsg_pop (msg);
                assert (strcmp (header, MDPC_CLIENT) == 0);
                free (header);

                char *service = zmsg_pop (msg);
                assert (strcmp (service, service) == 0);
                free (service);

                return msg;     //  Success
            }
            else
            if (--retries_left) {
                //  Reconnect, and resend message
                s_connect_to_broker (self);
                zmsg_t *msg = zmsg_dup (request);
                zmsg_push (msg, service);
                zmsg_push (msg, MDPC_CLIENT);
                zmsg_send (&msg, self->client);
            }
            else
                break;          //  Give up
        }
    }
    return NULL;
}
