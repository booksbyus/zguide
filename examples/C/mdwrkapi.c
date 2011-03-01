/*  =========================================================================
    mdwrkapi.c

    Majordomo Protocol Worker API
    Implements the MDP/Server spec at http://rfc.zeromq.org/spec:7.

    Follows the ZFL class conventions and is further developed as the ZFL
    mdwrk class.  See http://zfl.zeromq.org for more details.

    -------------------------------------------------------------------------
    Copyright (c) 1991-2011 iMatix Corporation <www.imatix.com>
    Copyright other contributors as noted in the AUTHORS file.

    This file is part of the ZeroMQ Guide: http://zguide.zeromq.org

    This is free software; you can redistribute it and/or modify it under the
    terms of the GNU Lesser General Public License as published by the Free
    Software Foundation; either version 3 of the License, or (at your option)
    any later version.

    This software is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABIL-
    ITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General
    Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
    =========================================================================
*/

#ifndef __MDWRKAPI_H_INCLUDED__
#define __MDWRKAPI_H_INCLUDED__

#include "zhelpers.h"
#include "zmsg.c"

//  This is the version of MDP/Server we implement
#define MDPS_HEADER         "MDPS01"

//  Reliability parameters
#define HEARTBEAT_LIVENESS  3       //  3-5 is reasonable
#define HEARTBEAT_INTERVAL  1000    //  msecs
#define INTERVAL_INIT       1000    //  Initial reconnect
#define INTERVAL_MAX       32000    //  After exponential backoff

//  Protocol commands
#define MDPS_READY          "\001"
#define MDPS_REQUEST        "\002"
#define MDPS_REPLY          "\003"
#define MDPS_HEARTBEAT      "\004"
#define MDPS_DISCONNECT     "\005"

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
    size_t interval;            //  Reconnect interval
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
    zmsg_append (msg, MDPS_HEADER);
    zmsg_append (msg, MDPS_READY);
    zmsg_append (msg, self->service);
    zmsg_send (&msg, self->worker);

    //  If liveness hits zero, queue is considered disconnected
    self->liveness = HEARTBEAT_LIVENESS;
    self->interval = INTERVAL_INIT;
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
//  Send reply, if any, to broker and wait for request.

zmsg_t *
mdwrk_recv (mdwrk_t *self, zmsg_t *reply)
{
        //  Prefix reply with protocol frames
        //  Frame 1: "MDPSxy" (six bytes, MDP/Server x.y)
        //  Frame 2: Service name (printable string)
//        zmsg_t *msg = zmsg_dup (request);
 //       zmsg_wrap (msg, MDPS_HEADER, service);
  //      zmsg_send (self->worker, &msg);

    - if reply not null,
        take copy
        prefix with frames
        send to broker

    - poll loop for input request
        return to caller

    while (1) {
        zmq_pollitem_t items [] = { { worker,  0, ZMQ_POLLIN, 0 } };
        zmq_poll (items, 1, HEARTBEAT_INTERVAL * 1000);

        if (items [0].revents & ZMQ_POLLIN) {
            //  Get message
            //  - 3-part envelope + content -> request
            //  - 1-part "HEARTBEAT" -> heartbeat
            zmsg_t *zmsg = zmsg_recv (worker);

            if (zmsg_parts (zmsg) == 3) {
                zmsg_send (&zmsg, worker);
                liveness = HEARTBEAT_LIVENESS;
            }
            else
            if (zmsg_parts (zmsg) == 1
            && strcmp (zmsg_body (zmsg), "HEARTBEAT") == 0)
                liveness = HEARTBEAT_LIVENESS;
            else {
                printf ("E: (%s) invalid message\n", identity);
                zmsg_dump (zmsg);
            }
            interval = INTERVAL_INIT;
        }
        else
        if (--liveness == 0) {
            printf ("W: (%s) heartbeat failure, can't reach queue\n",
                identity);
            printf ("W: (%s) reconnecting in %zd msec...\n",
                identity, interval);
            s_sleep (interval);

            if (interval < INTERVAL_MAX)
                interval *= 2;
            zmq_close (worker);
            worker = s_worker_socket (context);
            liveness = HEARTBEAT_LIVENESS;
        }

        //  Send heartbeat to queue if it's time
        if (s_clock () > self->heartbeat_at) {
            heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;
            printf ("I: (%s) worker heartbeat\n", identity);
            s_send (worker, "HEARTBEAT");
        }
    }


//    return request;
return NULL;
}
