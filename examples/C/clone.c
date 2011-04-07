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
    void *snapshot;             //  Snapshot socket
    void *subscriber;           //  Outgoing updates
    void *publisher;            //  Incoming updates
    int64_t expires;            //  Expires at this time
} server_t;

server_t *
server_new (zctx_t *ctx, char *address, int port)
{
    server_t *self = (server_t *) malloc (sizeof (server_t));

    printf ("I: connecting to %s:%d...\n", address, port);
    self->snapshot = zsocket_new (ctx, ZMQ_DEALER);
    zsocket_connect (self->snapshot, "%s:%d", address, port);
    self->subscriber = zsocket_new (ctx, ZMQ_SUB);
    zsocket_connect (self->subscriber, "%s:%d", address, port + 1);
    self->publisher = zsocket_new (ctx, ZMQ_PUB);
    zsocket_connect (self->publisher, "%s:%d", address, port + 2);

    self->expires = zclock_time () + SERVER_TTL;
    return self;
}

void
server_destroy (server_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        server_t *self = *self_p;
        free (self);
        *self_p = NULL;
    }
}

//  ---------------------------------------------------------------------
//  Our agent class

//  States we can be in
#define STATE_STARTUP       0   //  Not sent anything to a server
#define STATE_PENDING       1   //  Getting state from server
#define STATE_ACTIVE        2   //  Listening to subscriptions

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

agent_t *
agent_new (zctx_t *ctx, void *pipe)
{
    agent_t *self = (agent_t *) zmalloc (sizeof (agent_t));
    self->ctx = ctx;
    self->pipe = pipe;
    self->kvmap = zhash_new ();
    self->state = STATE_STARTUP;
    return self;
}

void
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
int
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

    while (!zctx_interrupted) {
        zmq_pollitem_t items [] = {
            { pipe, 0, ZMQ_POLLIN, 0 },
            { 0,    0, ZMQ_POLLIN, 0 }
        };
        server_t *server = self->server [self->cur_server];
        switch (self->state) {
            case STATE_STARTUP:
                if (self->nbr_servers > 0) {
                    self->state = STATE_PENDING;
                    server->expires = zclock_time () + SERVER_TTL;
                    zstr_send (server->snapshot, "ICANHAZ?");
                    //  And fall-through to STATE_PENDING
                }
                else
                    break;
            case STATE_PENDING:
                items [1].socket = server->snapshot;
                break;
            case STATE_ACTIVE:
                items [1].socket = server->subscriber;
                break;
        }
        //  ------------------------------------------------------------
        //  Poll loop
        int64_t expires = server? server->expires: 3600 * 1000;
        int tickless = (int) (expires - zclock_time ());
        if (tickless < 0)
            tickless = 0;
        int rc = zmq_poll (items, 2, tickless * 1000);
        if (rc == -1)
            break;              //  Context has been shut down

        if (items [0].revents & ZMQ_POLLIN) {
            if (agent_control_message (self))
                break;          //  Interrupted
        }
        if (items [1].revents & ZMQ_POLLIN) {
            server->expires = zclock_time () + SERVER_TTL;
            kvmsg_t *kvmsg = kvmsg_recv (items [1].socket);
            if (!kvmsg)
                break;          //  Interrupted

            if (self->state == STATE_PENDING) {
                //  Store in snapshot until we're finished
                if (strneq (kvmsg_key (kvmsg), "KTHXBAI"))
                    kvmsg_store (&kvmsg, self->kvmap);
                else {
                    self->sequence = kvmsg_sequence (kvmsg);
                    printf ("I: received snapshot=%" PRId64 "\n",
                        self->sequence);
                    kvmsg_destroy (&kvmsg);
                    self->state = STATE_ACTIVE;
                }
            }
            else
            if (self->state == STATE_ACTIVE) {
                //  Discard out-of-sequence updates, incl. hugz
                if (kvmsg_sequence (kvmsg) > self->sequence) {
                    self->sequence = kvmsg_sequence (kvmsg);
                    kvmsg_store (&kvmsg, self->kvmap);
                    printf ("I: received update=%" PRId64 "\n",
                        self->sequence);
                }
                else
                    kvmsg_destroy (&kvmsg);
            }
        }
        if (zclock_time () >= expires) {
            if (self->state == STATE_PENDING
            ||  self->state == STATE_ACTIVE) {
                //  Server has died, failover
                printf ("I: server didn't give hugz, fail-over...\n");
                self->cur_server = ++self->cur_server % self->nbr_servers;
                self->state = STATE_STARTUP;
            }
        }
    }
    agent_destroy (&self);
}
