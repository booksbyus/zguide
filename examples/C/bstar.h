/*  =========================================================================
    bstar - Binary Star pattern

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

#ifndef __BSTAR_H_INCLUDED__
#define __BSTAR_H_INCLUDED__

#include "zmsg.h"

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _bstar_t bstar_t;

//  States we can be in at any point in time
typedef enum {
    STATE_PRIMARY = 1,          //  Primary, waiting for peer to connect
    STATE_BACKUP = 2,           //  Backup, waiting for peer to connect
    STATE_ACTIVE = 3,           //  Active - accepting connections
    STATE_PASSIVE = 4           //  Passive - not accepting connections
} state_t;

//  Events, which start with the states our peer can be in
typedef enum {
    PEER_PRIMARY = 1,           //  HA peer is pending primary
    PEER_BACKUP = 2,            //  HA peer is pending backup
    PEER_ACTIVE = 3,            //  HA peer is active
    PEER_PASSIVE = 4,           //  HA peer is passive
    CLIENT_REQUEST = 5          //  Client makes request
} event_t;


//  We send state information every this often
//  If peer doesn't respond in two heartbeats, it is 'dead'
#define BSTAR_HEARTBEAT     1000        //  In msecs

//  Create a new Binary Star instance
bstar_t *
    bstar_new (int frontend_type);

//  Destroy a Binary Star instance
void
    bstar_destroy (bstar_t **self_p);

//  Establish peering, bind PUB and connect SUB
void
    bstar_peer (bstar_t *self, char *bind_to, char *connect_to);

//  Initialize Binary Star, bind frontend and set initial state
void
    bstar_init (bstar_t *self, char *bind_to, Bool primary);
    
//  Return frontend socket
void *
    bstar_frontend (bstar_t *self);
    
//  Receive message off frontend socket
zmsg_t *
    bstar_recv (bstar_t *self);
    
//  Send message to frontend socket
void
    bstar_send (bstar_t *self, zmsg_t **msg_p);
    
//  Execute state machine, return -1 on errors
int
    bstar_execute (bstar_t *self);

#ifdef __cplusplus
}
#endif


/*  =========================================================================
    bstar - Binary Star pattern

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

//  Structure of our class

struct _bstar_t {
    void *context;              //  0MQ context 
    void *statepub;             //  State publisher
    void *statesub;             //  State subscriber
    void *frontend;             //  Client frontend
    state_t state;              //  Current state
    event_t event;              //  Current event
    int64_t peer_expiry;        //  When peer is considered 'dead'
    int64_t send_state;         //  When we send our state
};


//  --------------------------------------------------------------------------
//  Constructor; if size is >0, allocates frame with that size, and if data
//  is not null, copies data into frame.

bstar_t *
bstar_new (int frontend_type)
{
    bstar_t
        *self;

    self = (bstar_t *) calloc (1, sizeof (bstar_t));

    self->context = zmq_init (1);
    self->statepub = zmq_socket (self->context, ZMQ_PUB);
    self->statesub = zmq_socket (self->context, ZMQ_SUB);
    self->frontend = zmq_socket (self->context, frontend_type);
    self->send_state = s_clock () + BSTAR_HEARTBEAT;
    s_catch_signals ();
    
    return self;
}


//  --------------------------------------------------------------------------
//  Destructor

void
bstar_destroy (bstar_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        bstar_t *self = *self_p;
        
        //  Shutdown sockets and context
        zmq_close (self->statepub);
        zmq_close (self->statesub);
        zmq_close (self->frontend);
        zmq_term (self->context);
        free (self);
        *self_p = NULL;
    }
}


//  --------------------------------------------------------------------------
//  Establishes peering, binding publisher and connecting subscriber.

void
bstar_peer (bstar_t *self, char *bind_to, char *connect_to)
{
    int rc = zmq_bind (self->statepub, bind_to);
    assert (rc == 0);
    zmq_setsockopt (self->statesub, ZMQ_SUBSCRIBE, "", 0);
    rc = zmq_connect (self->statesub, connect_to);
    assert (rc == 0);
}


//  --------------------------------------------------------------------------
//  Sets initial state for peering and accept client requests

void
bstar_init (bstar_t *self, char *bind_to, Bool primary)
{
    self->state = primary? STATE_PRIMARY: STATE_BACKUP;
    int rc = zmq_bind (self->frontend, bind_to);
    assert (rc == 0);
}


//  --------------------------------------------------------------------------
//  Return bstar frontend socket

void *
bstar_frontend (bstar_t *self)
{
    return self->frontend;
}


//  --------------------------------------------------------------------------
//  Wait for message on frontend, return new zmsg_t or NULL

zmsg_t *
bstar_recv (bstar_t *self)
{
    while (!s_interrupted) {
        zmq_pollitem_t items [] = { 
            { self->frontend, 0, ZMQ_POLLIN, 0 }, 
            { self->statesub, 0, ZMQ_POLLIN, 0 }
        };
        int time_left = (int) ((self->send_state - s_clock ()));
        if (time_left < 0)
            time_left = 0;
        int rc = zmq_poll (items, 2, time_left * 1000);
        if (rc == -1)
            break;              //  Context has been shut down

        if (items [0].revents & ZMQ_POLLIN) {
            //  Have a client request
            zmsg_t *msg = zmsg_recv (self->frontend);
            self->event = CLIENT_REQUEST;
            if (bstar_execute (self) == 0)
                return msg;
            else
                zmsg_destroy (&msg);
        }
        if (items [1].revents & ZMQ_POLLIN) {
            //  Have state from our peer, execute as event
            char *state = s_recv (self->statesub);
            self->event = atoi (state);
            free (state);
            if (bstar_execute (self))
                break;          //  Error, so exit
            self->peer_expiry = s_clock () + 2 * BSTAR_HEARTBEAT;
        }
        //  If we timed-out, send state to peer
        if (s_clock () >= self->send_state) {
            char state [2];
            sprintf (state, "%d", self->state);
            s_send (self->statepub, state);
            self->send_state = s_clock () + BSTAR_HEARTBEAT;
        }
    }
    return NULL;
}


//  --------------------------------------------------------------------------
//  Send message to frontend, destroy it when done

void
bstar_send (bstar_t *self, zmsg_t **msg_p)
{
    zmsg_send (msg_p, self->frontend);
}


//  --------------------------------------------------------------------------
//  Execute finite state machine (apply event to state)
//  Returns -1 if there was an exception, 0 if event was valid.

int
bstar_execute (bstar_t *self)
{
    int rc = 0;
    //  Primary server is waiting for peer to connect
    //  Accepts CLIENT_REQUEST events in this state
    if (self->state == STATE_PRIMARY) {
        if (self->event == PEER_BACKUP) {
            printf ("I: connected to backup (slave), ready as master\n");
            self->state = STATE_ACTIVE;
        }
        else
        if (self->event == PEER_ACTIVE) {
            printf ("I: connected to backup (master), ready as slave\n");
            self->state = STATE_PASSIVE;
        }
    }
    else
    //  Backup server is waiting for peer to connect
    //  Rejects CLIENT_REQUEST events in this state
    if (self->state == STATE_BACKUP) {
        if (self->event == PEER_ACTIVE) {
            printf ("I: connected to primary (master), ready as slave\n");
            self->state = STATE_PASSIVE;
        }
        else
        if (self->event == CLIENT_REQUEST)
            rc = -1;
    }
    else
    //  Server is active
    //  Accepts CLIENT_REQUEST events in this state
    if (self->state == STATE_ACTIVE) {
        if (self->event == PEER_ACTIVE) {
            //  Two masters would mean split-brain
            printf ("E: fatal error - dual masters, aborting\n");
            rc = -1;
        }
    }
    else
    //  Server is passive
    //  CLIENT_REQUEST events can trigger failover if peer looks dead
    if (self->state == STATE_PASSIVE) {
        if (self->event == PEER_PRIMARY) {
            //  Peer is restarting - become active, peer will go passive
            printf ("I: primary (slave) is restarting, ready as master\n");
            self->state = STATE_ACTIVE;
        }
        else
        if (self->event == PEER_BACKUP) {
            //  Peer is restarting - become active, peer will go passive
            printf ("I: backup (slave) is restarting, ready as master\n");
            self->state = STATE_ACTIVE;
        }
        else
        if (self->event == PEER_PASSIVE) {
            //  Two passives would mean cluster would be non-responsive
            printf ("E: fatal error - dual slaves, aborting\n");
            rc = -1;
        }
        else
        if (self->event == CLIENT_REQUEST) {
            //  Peer becomes master if timeout has passed
            //  It's the client request that triggers the failover
            assert (self->peer_expiry > 0);
            if (s_clock () > self->peer_expiry) {
                //  If peer is dead, switch to the active state
                printf ("I: failover successful, ready as master\n");
                self->state = STATE_ACTIVE;
            }
            else
                //  If peer is alive, reject connections
                rc = -1;
        }
    }
    return rc;
}

#endif
