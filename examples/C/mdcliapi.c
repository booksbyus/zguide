/*  =====================================================================
    mdcliapi.c

    Majordomo Protocol Client API
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

#include "mdcliapi.h"

//  Structure of our class
//  We access these properties only via class methods

struct _mdcli_t {
    char *broker;
    void *context;
    void *client;               //  Socket to broker
    int verbose;                //  Print activity to stdout
    int timeout;                //  Request timeout
    int retries;                //  Request retries
};


//  ---------------------------------------------------------------------
//  Connect or reconnect to broker

void s_mdcli_connect_to_broker (mdcli_t *self)
{
    if (self->client)
        zmq_close (self->client);
    self->client = zmq_socket (self->context, ZMQ_REQ);
    int linger = 0;
    zmq_setsockopt (self->client, ZMQ_LINGER, &linger, sizeof (linger));
    zmq_connect (self->client, self->broker);
    if (self->verbose)
        s_console ("I: connecting to broker at %s...", self->broker);
}


//  ---------------------------------------------------------------------
//  Constructor

mdcli_t *
mdcli_new (char *broker, int verbose)
{
    assert (broker);
    s_version_assert (2, 1);

    mdcli_t *self = (mdcli_t *) calloc (1, sizeof (mdcli_t));
    self->broker = strdup (broker);
    self->context = zmq_init (1);
    self->verbose = verbose;
    self->timeout = 2500;           //  msecs
    self->retries = 3;              //  Before we abandon

    s_catch_signals ();
    s_mdcli_connect_to_broker (self);
    return self;
}


//  ---------------------------------------------------------------------
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


//  ---------------------------------------------------------------------
//  Set request timeout

void
mdcli_set_timeout (mdcli_t *self, int timeout)
{
    assert (self);
    self->timeout = timeout;
}


//  ---------------------------------------------------------------------
//  Set request retries

void
mdcli_set_retries (mdcli_t *self, int retries)
{
    assert (self);
    self->retries = retries;
}


//  ---------------------------------------------------------------------
//  Send request to broker and get reply by hook or crook
//  Takes ownership of request message and destroys it when sent.
//  Returns the reply message or NULL if there was no reply.

zmsg_t *
mdcli_send (mdcli_t *self, char *service, zmsg_t **request_p)
{
    assert (self);
    assert (request_p);
    zmsg_t *request = *request_p;

    //  Prefix request with protocol frames
    //  Frame 1: "MDPCxy" (six bytes, MDP/Client x.y)
    //  Frame 2: Service name (printable string)
    zmsg_push (request, service);
    zmsg_push (request, MDPC_CLIENT);
    if (self->verbose) {
        s_console ("I: send request to '%s' service:", service);
        zmsg_dump (request);
    }

    int retries_left = self->retries;
    while (retries_left && !s_interrupted) {
        zmsg_t *msg = zmsg_dup (request);
        zmsg_send (&msg, self->client);

        while (!s_interrupted) {
            //  Poll socket for a reply, with timeout
            zmq_pollitem_t items [] = { 
                { self->client, 0, ZMQ_POLLIN, 0 } };
            zmq_poll (items, 1, self->timeout * 1000);

            //  If we got a reply, process it
            if (items [0].revents & ZMQ_POLLIN) {
                zmsg_t *msg = zmsg_recv (self->client);
                if (self->verbose) {
                    s_console ("I: received reply:");
                    zmsg_dump (msg);
                }
                //  Don't try to handle errors, just assert noisily
                assert (zmsg_parts (msg) >= 3);

                char *header = zmsg_pop (msg);
                assert (strcmp (header, MDPC_CLIENT) == 0);
                free (header);

                char *reply_service = zmsg_pop (msg);
                assert (strcmp (reply_service, service) == 0);
                free (reply_service);

                zmsg_destroy (&request);
                return msg;     //  Success
            }
            else
            if (--retries_left) {
                if (self->verbose)
                    s_console ("W: no reply, reconnecting...");
                //  Reconnect, and resend message
                s_mdcli_connect_to_broker (self);
                zmsg_t *msg = zmsg_dup (request);
                zmsg_send (&msg, self->client);
            }
            else {
                if (self->verbose)
                    s_console ("W: permanent error, abandoning request");
                break;          //  Give up
            }
        }
    }
    if (s_interrupted)
        printf ("W: interrupt received, killing client...\n");
    zmsg_destroy (&request);
    return NULL;
}
