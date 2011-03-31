/*  =========================================================================
    bstar - Binary Star server core

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

#include "bstar.h"
#include "zlist.h"

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

//  Structure of our class

struct _bstar_t {
    void *context;              //  0MQ context
    void *statepub;             //  State publisher
    void *statesub;             //  State subscriber
    zlist_t *frontends;         //  List of frontends
    state_t state;              //  Current state
    event_t event;              //  Current event
    int64_t peer_expiry;        //  When peer is considered 'dead'
    int64_t send_state;         //  When we send our state
};


//  --------------------------------------------------------------------------
//  Constructor; if size is >0, allocates frame with that size, and if data
//  is not null, copies data into frame.

bstar_t *
bstar_new (int primary, char *bind_to, char *connect_to)
{
    bstar_t
        *self;

    self = (bstar_t *) calloc (1, sizeof (bstar_t));

    //  Initialize the Binary Star
    self->state = primary? STATE_PRIMARY: STATE_BACKUP;
    self->frontends = zlist_new ();
    self->send_state = s_clock () + BSTAR_HEARTBEAT;
    s_catch_signals ();

    //  We'll manage our own 0MQ context and sockets
    self->context = zmq_init (1);

    //  Create publisher for state going to peer
    self->statepub = zmq_socket (self->context, ZMQ_PUB);
    int rc = zmq_bind (self->statepub, bind_to);
    assert (rc == 0);

    //  Create subscriber for state coming from peer
    self->statesub = zmq_socket (self->context, ZMQ_SUB);
    zmq_setsockopt (self->statesub, ZMQ_SUBSCRIBE, "", 0);
    rc = zmq_connect (self->statesub, connect_to);
    assert (rc == 0);

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
        int zero = 0;
        zmq_setsockopt (self->statepub, ZMQ_LINGER, &zero, sizeof (zero));
        zmq_close (self->statepub);

        zmq_setsockopt (self->statesub, ZMQ_LINGER, &zero, sizeof (zero));
        zmq_close (self->statesub);

        //  Close all frontends, no lingering
        while (zlist_size (self->frontends)) {
            void *frontend = zlist_pop (self->frontends);
            zmq_setsockopt (frontend, ZMQ_LINGER, &zero, sizeof (zero));
            zmq_close (frontend);
        }
        zlist_destroy (&self->frontends);
        zmq_term (self->context);
        free (self);
        *self_p = NULL;
    }
}


//  --------------------------------------------------------------------------
//  Listen on this endpoint for client votes

void
bstar_listen (bstar_t *self, char *endpoint, int type)
{
    void *frontend = zmq_socket (self->context, type);
    int rc = zmq_bind (frontend, endpoint);
    assert (rc == 0);
    zlist_append (self->frontends, frontend);
}


//  --------------------------------------------------------------------------
//  Execute finite state machine (apply event to state)
//  Returns -1 if there was an exception, 0 if event was valid.

static int
s_execute_fsm (bstar_t *self)
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


//  --------------------------------------------------------------------------
//  Wait for valid activity on client socket, return socket

void *
bstar_wait (bstar_t *self)
{
    //  Build poll set
    int poll_size = zlist_size (self->frontends) + 1;
    zmq_pollitem_t *poll_set
        = calloc (1, poll_size * sizeof (zmq_pollitem_t));
    poll_set [0].socket = self->statesub;
    poll_set [0].events = ZMQ_POLLIN;
    uint index = 0;
    void *frontend = zlist_first (self->frontends);
    while (frontend) {
        index++;
        poll_set [index].socket = frontend;
        poll_set [index].events = ZMQ_POLLIN;
        frontend = zlist_next (self->frontends);
    }

    //  Handle socket activity until we get a valid client request
    frontend = NULL;
    while (!s_interrupted && !frontend) {
        int time_left = (int) ((self->send_state - s_clock ()));
        if (time_left < 0)
            time_left = 0;
        int rc = zmq_poll (poll_set, poll_size, time_left * 1000);
        if (rc == -1)
            break;              //  Context has been shut down

        if (poll_set [0].revents & ZMQ_POLLIN) {
            //  Have state from our peer, execute as event
            char *state = s_recv (self->statesub);
            self->event = atoi (state);
            free (state);
            if (s_execute_fsm (self))
                break;          //  Error, so exit
            self->peer_expiry = s_clock () + 2 * BSTAR_HEARTBEAT;
        }
        for (index = 1; index < poll_size; index++) {
            if (poll_set [index].revents & ZMQ_POLLIN) {
                //  Have a client request
                self->event = CLIENT_REQUEST;
                if (s_execute_fsm (self) == 0) {
                    frontend = poll_set [index].socket;
                    break;
                }
            }
        }
        //  If we timed-out, send state to peer
        if (s_clock () >= self->send_state) {
            char state [2];
            sprintf (state, "%d", self->state);
            s_send (self->statepub, state);
            self->send_state = s_clock () + BSTAR_HEARTBEAT;
        }
    }
    free (poll_set);
    return frontend;
}
