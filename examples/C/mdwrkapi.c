//
//  Majordomo Protocol Worker API
//  Implements the MDP/Worker spec at http://rfc.zeromq.org/spec:7.
//
#include "zhelpers.h"
#include "zmsg.c"
#include "mdp.h"

//  Reliability parameters
#define HEARTBEAT_LIVENESS  3       //  3-5 is reasonable
#define HEARTBEAT_INTERVAL  5000    //  msecs
#define RECONNECT_INTERVAL  2500    //  Delay between reconnects

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _mdwrk_t mdwrk_t;

mdwrk_t *
    mdwrk_new (char *broker,char *service, int verbose);
void
    mdwrk_destroy (mdwrk_t **self_p);
zmsg_t *
    mdwrk_recv (mdwrk_t *self, zmsg_t *reply);

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
    int verbose;                //  Print activity to stdout

    //  Heartbeat management
    uint64_t heartbeat_at;      //  When to send HEARTBEAT
    size_t liveness;            //  How many attempts left

    //  Internal state
    int expect_reply;           //  Zero only at start
};


//  --------------------------------------------------------------------------
//  Send message to broker
//  If no _msg is provided, creates one internally

static void
s_send_to_broker (mdwrk_t *self, char *command, char *option, zmsg_t *_msg)
{
    zmsg_t *msg = _msg? zmsg_dup (_msg): zmsg_new ();

    //  Stack protocol envelope to start of message
    if (option)
        zmsg_push (msg, option);
    zmsg_push (msg, command);
    zmsg_push (msg, MDPS_WORKER);

    if (self->verbose) {
        s_console ("I: sending %s to broker",
            mdps_commands [(int) *command]);
        zmsg_dump (msg);
    }
    zmsg_send (&msg, self->worker);
}


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
    if (self->verbose)
        s_console ("I: connecting to broker at %s...", self->broker);

    //  Register service with broker
    s_send_to_broker (self, MDPS_READY, self->service, NULL);

    //  If liveness hits zero, queue is considered disconnected
    self->liveness = HEARTBEAT_LIVENESS;
    self->heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;
}


//  --------------------------------------------------------------------------
//  Constructor

mdwrk_t *
mdwrk_new (char *broker,char *service, int verbose)
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
    self->verbose = verbose;
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
    if (reply)
        s_send_to_broker (self, MDPS_REPLY, NULL, reply);
    self->expect_reply = 1;

    while (1) {
        zmq_pollitem_t items [] = { { self->worker,  0, ZMQ_POLLIN, 0 } };
        zmq_poll (items, 1, HEARTBEAT_INTERVAL * 1000);

        if (items [0].revents & ZMQ_POLLIN) {
            zmsg_t *msg = zmsg_recv (self->worker);
            if (self->verbose) {
                s_console ("I: received input message:");
                zmsg_dump (msg);
            }
            self->liveness = HEARTBEAT_LIVENESS;

            //  Don't try to handle errors, just assert noisily
            assert (zmsg_parts (msg) >= 2);

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
                s_console ("E: invalid input message (%d)", (int) *command);
                zmsg_dump (msg);
            }
            free (command);
        }
        else
        if (--self->liveness == 0) {
            if (self->verbose)
                s_console ("W: disconnected from broker - retrying...");
            s_sleep (RECONNECT_INTERVAL);
            s_connect_to_broker (self);
        }
        //  Send HEARTBEAT if it's time
        if (s_clock () > self->heartbeat_at) {
            s_send_to_broker (self, MDPS_HEARTBEAT, NULL, NULL);
            self->heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;
        }
    }
    //  We exit if we've been disconnected
    return NULL;
}
