//
//  Majordomo Protocol Worker API
//  Implements the MDP/Worker spec at http://rfc.zeromq.org/spec:7.
//
#include "zmsg.class"
#include "mdp.h"

//  Reliability parameters
#define HEARTBEAT_LIVENESS  3       //  3-5 is reasonable
#define HEARTBEAT_INTERVAL  1000    //  msecs
#define RECONNECT_INTERVAL  1000    //  Delay between attempts

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _mdwrk_t mdwrk_t;

mdwrk_t *mdwrk_new     (char *broker,char *service);
void     mdwrk_destroy (mdwrk_t **self_p);
zmsg_t  *mdwrk_recv    (mdwrk_t *self, zmsg_t *reply);

#ifdef __cplusplus
}
#endif

//  Structure of our class
//  We access these properties only via class methods

struct _mdwrk_t {
    char *broker;
    char *service;
    void *context;
    void *worker;               //  Socket to broker

    //  Heartbeat management
    uint64_t heartbeat_at;      //  When to send HEARTBEAT
    size_t liveness;            //  How many attempts left

    //  Internal state
    int expect_reply;           //  Zero only at start
};


//  --------------------------------------------------------------------------
//  Connect or reconnect to broker

void s_connect_to_broker (mdwrk_t *self)
{
    if (self->worker)
        zmq_close (self->worker);
    self->worker = zmq_socket (self->context, ZMQ_XREQ);
    int linger = 0;
    zmq_setsockopt (self->worker, ZMQ_LINGER, &linger, sizeof (linger));
    zmq_connect (self->worker, self->broker);

    //  Register service with broker
    zmsg_t *msg = zmsg_new ();
    zmsg_append (msg, MDPS_WORKER);
    zmsg_append (msg, MDPS_READY);
    zmsg_append (msg, self->service);
    zmsg_send (&msg, self->worker);

    //  If liveness hits zero, queue is considered disconnected
    self->liveness = HEARTBEAT_LIVENESS;
    self->heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;
}


//  --------------------------------------------------------------------------
//  Constructor

mdwrk_t *
mdwrk_new (char *broker,char *service)
{
    mdwrk_t
        *self;

    assert (broker);
    assert (service);
    s_version_assert (2, 1);
    self = malloc (sizeof (mdwrk_t));
    memset (self, 0, sizeof (mdwrk_t));

    self->broker = strdup (broker);
    self->service = strdup (service);
    self->context = zmq_init (1);
    s_connect_to_broker (self);
    return (self);
}


//  --------------------------------------------------------------------------
//  Destructor

void
mdwrk_destroy (mdwrk_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        mdwrk_t *self = *self_p;
        zmq_close (self->worker);
        zmq_term (self->context);
        free (self->broker);
        free (self->service);
        free (self);
        *self_p = NULL;
    }
}


//  --------------------------------------------------------------------------
//  Send reply, if any, to broker and wait for next request.

zmsg_t *
mdwrk_recv (mdwrk_t *self, zmsg_t *reply)
{
    //  Format and send the reply if we were provided one
    assert (reply || !self->expect_reply);
    if (reply) {
        zmsg_t *msg = zmsg_dup (reply);
        zmsg_push (msg, MDPS_REPLY);
        zmsg_push (msg, MDPS_WORKER);
        zmsg_send (&msg, self->worker);
    }
    self->expect_reply = 1;

    while (1) {
        zmq_pollitem_t items [] = { { self->worker,  0, ZMQ_POLLIN, 0 } };
        zmq_poll (items, 1, HEARTBEAT_INTERVAL * 1000);

        if (items [0].revents & ZMQ_POLLIN) {
            zmsg_t *msg = zmsg_recv (self->worker);
            self->liveness = HEARTBEAT_LIVENESS;

            //  Don't try to handle errors, just assert noisily
            assert (zmsg_parts (msg) >= 3);

            char *header = zmsg_pop (msg);
            assert (strcmp (header, MDPS_WORKER) == 0);
            free (header);

            char *command = zmsg_pop (msg);
            if (strcmp (command, MDPS_REQUEST) == 0)
                return msg;     //  We have a request to process
            else
            if (strcmp (command, MDPS_HEARTBEAT) == 0)
                ;               //  Do nothing for heartbeats
            else
            if (strcmp (command, MDPS_DISCONNECT) == 0)
                break;          //  Return empty handed
            else {
                printf ("E: invalid input message (%d)\n", (int) command [1]);
                zmsg_dump (msg);
            }
            free (command);
        }
        else
        if (--self->liveness == 0) {
            s_sleep (RECONNECT_INTERVAL);
            s_connect_to_broker (self);
        }
        //  Send HEARTBEAT if it's time
        if (s_clock () > self->heartbeat_at) {
            zmsg_t *msg = zmsg_new ();
            zmsg_append (msg, MDPS_WORKER);
            zmsg_append (msg, MDPS_HEARTBEAT);
            zmsg_send (&msg, self->worker);
            self->heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;
        }
    }
    //  We exit if we've been disconnected
    return NULL;
}
