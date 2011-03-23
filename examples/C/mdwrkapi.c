/*  =====================================================================
    mdwrkapi.c

    Majordomo Protocol Worker API
    Implements the MDP/Worker spec at http://rfc.zeromq.org/spec:7.

    ---------------------------------------------------------------------
    Copyright (c) 1991-2011 iMatix Corporation <www.imatix.com>
    Copyright other contributors as noted in the AUTHORS file.

    This file is part of the ZeroMQ Guide: http://zguide.zeromq.org

    This is free software; you can redistribute it and/or modify it under
    the terms of the GNU Lesser General Public License as published by 
    the Free Software Foundation; either version 3 of the License, or (at 
    your option) any later version.

    This software is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of 
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public 
    License along with this program. If not, see 
    <http://www.gnu.org/licenses/>.
    =====================================================================
*/

#include "mdwrkapi.h"

//  Reliability parameters
#define HEARTBEAT_LIVENESS  3       //  3-5 is reasonable

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
    int heartbeat;              //  Heartbeat delay, msecs
    int reconnect;              //  Reconnect delay, msecs

    //  Internal state
    int expect_reply;           //  Zero only at start

    //  Return address, if any
    char *reply_to;
};


//  ---------------------------------------------------------------------
//  Send message to broker
//  If no _msg is provided, creates one internally

static void
s_mdwrk_send_to_broker (mdwrk_t *self, char *command, char *option, 
                        zmsg_t *_msg)
{
    zmsg_t *msg = _msg? zmsg_dup (_msg): zmsg_new (NULL);

    //  Stack protocol envelope to start of message
    if (option)
        zmsg_push (msg, option);
    zmsg_push (msg, command);
    zmsg_push (msg, MDPW_WORKER);
    zmsg_push (msg, "");

    if (self->verbose) {
        s_console ("I: sending %s to broker",
            mdps_commands [(int) *command]);
        zmsg_dump (msg);
    }
    zmsg_send (&msg, self->worker);
}


//  ---------------------------------------------------------------------
//  Connect or reconnect to broker

void s_mdwrk_connect_to_broker (mdwrk_t *self)
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
    s_mdwrk_send_to_broker (self, MDPW_READY, self->service, NULL);

    //  If liveness hits zero, queue is considered disconnected
    self->liveness = HEARTBEAT_LIVENESS;
    self->heartbeat_at = s_clock () + self->heartbeat;
}


//  ---------------------------------------------------------------------
//  Constructor

mdwrk_t *
mdwrk_new (char *broker,char *service, int verbose)
{
    assert (broker);
    assert (service);
    s_version_assert (2, 1);

    mdwrk_t *self = (mdwrk_t *) calloc (1, sizeof (mdwrk_t));
    self->broker = strdup (broker);
    self->service = strdup (service);
    self->context = zmq_init (1);
    self->verbose = verbose;
    self->heartbeat = 2500;     //  msecs
    self->reconnect = 2500;     //  msecs

    s_catch_signals ();
    s_mdwrk_connect_to_broker (self);
    return self;
}


//  ---------------------------------------------------------------------
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


//  ---------------------------------------------------------------------
//  Set heartbeat delay

void
mdwrk_set_heartbeat (mdwrk_t *self, int heartbeat)
{
    self->heartbeat = heartbeat;
}


//  ---------------------------------------------------------------------
//  Set reconnect delay

void
mdwrk_set_reconnect (mdwrk_t *self, int reconnect)
{
    self->reconnect = reconnect;
}


//  ---------------------------------------------------------------------
//  Send reply, if any, to broker and wait for next request.

zmsg_t *
mdwrk_recv (mdwrk_t *self, zmsg_t **reply_p)
{
    //  Format and send the reply if we were provided one
    assert (reply_p);
    zmsg_t *reply = *reply_p;
    assert (reply || !self->expect_reply);
    if (reply) {
        assert (self->reply_to);
        zmsg_wrap (reply, self->reply_to, "");
        free (self->reply_to);
        s_mdwrk_send_to_broker (self, MDPW_REPLY, NULL, reply);
        zmsg_destroy (reply_p);
    }
    self->expect_reply = 1;

    while (!s_interrupted) {
        zmq_pollitem_t items [] = { 
            { self->worker,  0, ZMQ_POLLIN, 0 } };
        zmq_poll (items, 1, self->heartbeat * 1000);

        if (items [0].revents & ZMQ_POLLIN) {
            zmsg_t *msg = zmsg_recv (self->worker);
            if (self->verbose) {
                s_console ("I: received message from broker:");
                zmsg_dump (msg);
            }
            self->liveness = HEARTBEAT_LIVENESS;

            //  Don't try to handle errors, just assert noisily
            assert (zmsg_parts (msg) >= 3);

            char *empty = zmsg_pop (msg);
            assert (strcmp (empty, "") == 0);
            free (empty);

            char *header = zmsg_pop (msg);
            assert (strcmp (header, MDPW_WORKER) == 0);
            free (header);

            char *command = zmsg_pop (msg);
            if (strcmp (command, MDPW_REQUEST) == 0) {
                //  We should pop and save as many addresses as there are
                //  up to a null part, but for now, just save one...
                self->reply_to = zmsg_unwrap (msg);
                free (command);
                return msg;     //  We have a request to process
            }
            else
            if (strcmp (command, MDPW_HEARTBEAT) == 0)
                ;               //  Do nothing for heartbeats
            else
            if (strcmp (command, MDPW_DISCONNECT) == 0)
                s_mdwrk_connect_to_broker (self);
            else {
                s_console ("E: invalid input message (%d)", 
                    (int) *command);
                zmsg_dump (msg);
            }
            free (command);
            zmsg_destroy (&msg);
        }
        else
        if (--self->liveness == 0) {
            if (self->verbose)
                s_console ("W: disconnected from broker - retrying...");
            s_sleep (self->reconnect);
            s_mdwrk_connect_to_broker (self);
        }
        //  Send HEARTBEAT if it's time
        if (s_clock () > self->heartbeat_at) {
            s_mdwrk_send_to_broker (self, MDPW_HEARTBEAT, NULL, NULL);
            self->heartbeat_at = s_clock () + self->heartbeat;
        }
    }
    if (s_interrupted)
        printf ("W: interrupt received, killing worker...\n");
    return NULL;
}
