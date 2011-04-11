/*  =====================================================================
    clone - client-side Clone Pattern class

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

#include "clone.h"

//  If no server replies within this time, abandon request
#define GLOBAL_TIMEOUT  4000    //  msecs
//  Server considered dead if silent for this long
#define SERVER_TTL      2000    //  msecs
//  Number of servers we will talk to
#define SERVER_MAX      2


//  =====================================================================
//  Synchronous part, works in our application thread

//  ---------------------------------------------------------------------
//  Structure of our class

struct _clone_t {
    zctx_t *ctx;        //  Our context wrapper
    void *pipe;         //  Pipe through to clone agent
};

//  This is the thread that handles our real clone class
static void clone_agent (void *args, zctx_t *ctx, void *pipe);


//  ---------------------------------------------------------------------
//  Constructor

clone_t *
clone_new (void)
{
    clone_t
        *self;

    self = (clone_t *) zmalloc (sizeof (clone_t));
    self->ctx = zctx_new ();
    self->pipe = zthread_fork (self->ctx, clone_agent, NULL);
    return self;
}

//  ---------------------------------------------------------------------
//  Destructor

void
clone_destroy (clone_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        clone_t *self = *self_p;
        zctx_destroy (&self->ctx);
        free (self);
        *self_p = NULL;
    }
}

//  ---------------------------------------------------------------------
//  Connect to new server endpoint
//  Sends [CONNECT][endpoint][service] to the agent

void
clone_connect (clone_t *self, char *address, char *service)
{
    assert (self);
    zmsg_t *msg = zmsg_new ();
    zmsg_addstr (msg, "CONNECT");
    zmsg_addstr (msg, address);
    zmsg_addstr (msg, service);
    zmsg_send (&msg, self->pipe);
}

//  ---------------------------------------------------------------------
//  Set new value in distributed hash table
//  Sends [SET][key][value] to the agent

void
clone_set (clone_t *self, char *key, char *value)
{
    assert (self);
    zmsg_t *msg = zmsg_new ();
    zmsg_addstr (msg, "SET");
    zmsg_addstr (msg, key);
    zmsg_addstr (msg, value);
    zmsg_send (&msg, self->pipe);
}

//  ---------------------------------------------------------------------
//  Lookup value in distributed hash table
//  Sends [GET][key] to the agent and waits for a value response
//  If there is no clone available, will eventually return NULL.

char *
clone_get (clone_t *self, char *key)
{
    assert (self);
    assert (key);
    zmsg_t *msg = zmsg_new ();
    zmsg_addstr (msg, "GET");
    zmsg_addstr (msg, key);
    zmsg_send (&msg, self->pipe);

    zmsg_t *reply = zmsg_recv (self->pipe);
    if (reply) {
        char *value = zmsg_popstr (reply);
        zmsg_destroy (&reply);
        return value;
    }
    return NULL;
}


//  =====================================================================
//  Asynchronous part, works in the background

//  ---------------------------------------------------------------------
//  Simple class for one server we talk to

typedef struct {
    char *address;              //  Server address
    int port;                   //  Server port
    void *snapshot;             //  Snapshot socket
    void *subscriber;           //  Outgoing updates
    void *publisher;            //  Incoming updates
    uint64_t expiry;            //  When server expires
} server_t;

static server_t *
server_new (zctx_t *ctx, char *address, int port)
{
    server_t *self = (server_t *) malloc (sizeof (server_t));

    printf ("I: adding server %s:%d...\n", address, port);
    self->address = strdup (address);
    self->port = port;

    self->snapshot = zsocket_new (ctx, ZMQ_DEALER);
    zsocket_connect (self->snapshot, "%s:%d", address, port);
    self->subscriber = zsocket_new (ctx, ZMQ_SUB);
    zsocket_connect (self->subscriber, "%s:%d", address, port + 1);
    self->publisher = zsocket_new (ctx, ZMQ_PUB);
    zsocket_connect (self->publisher, "%s:%d", address, port + 2);
    return self;
}

static void
server_destroy (server_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        server_t *self = *self_p;
        free (self->address);
        free (self);
        *self_p = NULL;
    }
}

//  ---------------------------------------------------------------------
//  Our agent class

//  States we can be in
#define STATE_INITIAL       0   //  Before asking server for state
#define STATE_WAITING       1   //  Waiting for state from server
#define STATE_SYNCING       2   //  Getting state from server
#define STATE_ACTIVE        3   //  Getting new updates from server

typedef struct {
    zctx_t *ctx;                //  Context wrapper
    void *pipe;                 //  Pipe back to application
    zhash_t *kvmap;             //  Actual key/value table
    server_t *server [SERVER_MAX];
    uint nbr_servers;           //  0 to SERVER_MAX
    uint state;                 //  Current state
    uint cur_server;            //  If active, server 0 or 1
    int64_t sequence;           //  Last kvmsg processed
} agent_t;

static agent_t *
agent_new (zctx_t *ctx, void *pipe)
{
    agent_t *self = (agent_t *) zmalloc (sizeof (agent_t));
    self->ctx = ctx;
    self->pipe = pipe;
    self->kvmap = zhash_new ();
    self->state = STATE_INITIAL;
    return self;
}

static void
agent_destroy (agent_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        agent_t *self = *self_p;
        int server_nbr;
        for (server_nbr = 0; server_nbr < self->nbr_servers; server_nbr++)
            server_destroy (&self->server [server_nbr]);
        zhash_destroy (&self->kvmap);
        free (self);
        *self_p = NULL;
    }
}

//  Returns -1 if thread was interrupted
static int
agent_control_message (agent_t *self)
{
    zmsg_t *msg = zmsg_recv (self->pipe);
    char *command = zmsg_popstr (msg);
    if (command == NULL)
        return -1;

    if (streq (command, "CONNECT")) {
        char *address = zmsg_popstr (msg);
        char *service = zmsg_popstr (msg);
        if (self->nbr_servers < SERVER_MAX)
            self->server [self->nbr_servers++]
                = server_new (self->ctx, address, atoi (service));
        else
            printf ("E: too many servers (max. %d)\n", SERVER_MAX);
        free (address);
        free (service);
    }
    else
    if (streq (command, "SET")) {
        char *key = zmsg_popstr (msg);
        //  We're doing string values for now, will switch to frames
        //  when we move this to libzapi.
        char *value = zmsg_popstr (msg);
        zhash_update (self->kvmap, key, (byte *) value);
        zhash_freefn (self->kvmap, key, free);

        //  Send key-value pair on to server
        kvmsg_t *kvmsg = kvmsg_new (0);
        kvmsg_set_key  (kvmsg, key);
        kvmsg_set_uuid (kvmsg);
        kvmsg_fmt_body (kvmsg, "%s", value);
        kvmsg_send (kvmsg, self->server [self->cur_server]->publisher);
        kvmsg_destroy (&kvmsg);

        free (key);             //  Value is owned by hash table
    }
    else
    if (streq (command, "GET")) {
        char *key = zmsg_popstr (msg);
        char *value = zhash_lookup (self->kvmap, key);
        if (value)
            zstr_send (self->pipe, value);
        else
            zstr_send (self->pipe, "");
        free (key);
        free (value);
    }
    free (command);
    zmsg_destroy (&msg);
    return 0;
}


//  ---------------------------------------------------------------------
//  Asynchronous agent manages server pool and handles request/reply
//  dialog when the application asks for it.

static void
clone_agent (void *args, zctx_t *ctx, void *pipe)
{
    agent_t *self = agent_new (ctx, pipe);

    while (TRUE) {
        zmq_pollitem_t poll_set [] = {
            { pipe, 0, ZMQ_POLLIN, 0 },
            { 0,    0, ZMQ_POLLIN, 0 }
        };
        int poll_timer = -1;
        int poll_size = 2;
        server_t *server = self->server [self->cur_server];
        switch (self->state) {
            case STATE_INITIAL:
                puts ("INITIAL");
                //  In this state we ask the server for a snapshot,
                //  if we have a server to talk to...
                if (self->nbr_servers > 0) {
                    printf ("I: waiting for server at %s:%d...\n",
                        server->address, server->port);
                    zstr_send (server->snapshot, "ICANHAZ?");
                    self->state = STATE_WAITING;
                    poll_set [1].socket = server->snapshot;
                }
                else
                    poll_size = 1;
                break;
            case STATE_WAITING:
                puts ("WAITING");
                //  In this state we read from snapshot and we can
                //  wait forever for an answer, it's not a failure.
                poll_set [1].socket = server->snapshot;
                break;
            case STATE_SYNCING:
                puts ("SYNCING");
                //  In this state we read from snapshot and we expect
                //  the server to respond, else we fail over.
                poll_set [1].socket = server->snapshot;
                poll_timer = (server->expiry - zclock_time ())
                           * ZMQ_POLL_MSEC;
                if (poll_timer < 0)
                    poll_timer = 0;
                break;
            case STATE_ACTIVE:
                puts ("ACTIVE");
                //  In this state we read from subscriber and we expect
                //  the server to give hugz, else we fail over.
                poll_set [1].socket = server->subscriber;
                poll_timer = (server->expiry - zclock_time ())
                           * ZMQ_POLL_MSEC;
                if (poll_timer < 0)
                    poll_timer = 0;
                break;
        }
        //  ------------------------------------------------------------
        //  Poll loop
        int rc = zmq_poll (poll_set, poll_size, poll_timer);
        if (rc == -1)
            break;              //  Context has been shut down

        if (poll_set [0].revents & ZMQ_POLLIN) {
            if (agent_control_message (self))
                break;          //  Interrupted
        }
        else
        if (poll_set [1].revents & ZMQ_POLLIN) {
            kvmsg_t *kvmsg = kvmsg_recv (poll_set [1].socket);
            if (!kvmsg)
                break;          //  Interrupted

            //  Anything from server resets its expiry time
            server->expiry = zclock_time () + SERVER_TTL;
            if (self->state == STATE_WAITING
            ||  self->state == STATE_SYNCING) {
                //  Store in snapshot until we're finished
                if (streq (kvmsg_key (kvmsg), "KTHXBAI")) {
                    printf ("I: received from %s:%d snapshot=%d\n",
                        server->address, server->port,
                        (int) self->sequence);
                    self->sequence = kvmsg_sequence (kvmsg);
                    kvmsg_destroy (&kvmsg);
                    self->state = STATE_ACTIVE;
                }
                else {
                    kvmsg_store (&kvmsg, self->kvmap);
                    self->state = STATE_SYNCING;
                }
            }
            else
            if (self->state == STATE_ACTIVE) {
                //  Discard out-of-sequence updates, incl. hugz
                if (kvmsg_sequence (kvmsg) > self->sequence) {
                    self->sequence = kvmsg_sequence (kvmsg);
                    kvmsg_store (&kvmsg, self->kvmap);
                    printf ("I: received from %s:%d update=%d\n",
                        server->address, server->port,
                        (int) self->sequence);
                }
                else
                    kvmsg_destroy (&kvmsg);
            }
        }
        else {
            //  Server has died, failover to next
            printf ("I: server at %s:%d didn't give hugz\n",
                    server->address, server->port);
            self->cur_server = (self->cur_server + 1) % self->nbr_servers;
            self->state = STATE_INITIAL;
        }
    }
    agent_destroy (&self);
}
