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
    void *context;      //  Our 0MQ context
    void *control;      //  Inproc socket talking to clone task
};

static void *clone_agent (void *context);

//  ---------------------------------------------------------------------
//  Constructor

clone_t *
clone_new (void)
{
    clone_t
        *self;

    s_catch_signals ();
    self = (clone_t *) malloc (sizeof (clone_t));
    self->context = zmq_init (1);
    self->control = zmq_socket (self->context, ZMQ_PAIR);
    int rc = zmq_bind (self->control, "inproc://clone");
    assert (rc == 0);

    pthread_t thread;
    pthread_create (&thread, NULL, clone_agent, self->context);
    pthread_detach (thread);

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
        int zero = 0;
        zmq_setsockopt (self->control,
            ZMQ_LINGER, &zero, sizeof (zero));
        zmq_close (self->control);
        zmq_term (self->context);
        free (self);
        *self_p = NULL;
    }
}

//  ---------------------------------------------------------------------
//  Connect to new server endpoint
//  Sends [CONNECT][address][service] to the agent

void
clone_connect (clone_t *self, char *address, char *service)
{
    assert (self);
    assert (address);
    assert (service);
    zmsg_t *msg = zmsg_new (service);
    zmsg_push (msg, address);
    zmsg_push (msg, "CONNECT");
    zmsg_send (&msg, self->control);
}

//  ---------------------------------------------------------------------
//  Set new value in distributed hash table
//  Sends [SET][key][value] to the agent

void
clone_set (clone_t *self, char *key, char *value)
{
    assert (self);
    assert (key);
    assert (value);
    zmsg_t *msg = zmsg_new (value);
    zmsg_push (msg, key);
    zmsg_push (msg, "SET");
    zmsg_send (&msg, self->control);
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
    zmsg_t *msg = zmsg_new (key);
    zmsg_push (msg, "GET");
    zmsg_send (&msg, self->control);

    zmsg_t *reply = zmsg_recv (self->control);
    if (reply) {
        char *value = zmsg_pop (reply);
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
server_new (void *context, char *address, char *service)
{
    server_t *self = (server_t *) malloc (sizeof (server_t));

    char endpoint [256];
    printf ("I: connecting to %s:%s...\n", address, service);
    snprintf (endpoint, 256, "%s:%d", address, atoi (service));
    self->snapshot = zmq_socket (context, ZMQ_DEALER);
    int rc = zmq_connect (self->snapshot, endpoint);
    assert (rc == 0);

    snprintf (endpoint, 256, "%s:%d", address, atoi (service) + 1);
    self->subscriber = zmq_socket (context, ZMQ_SUB);
    zmq_setsockopt (self->subscriber, ZMQ_SUBSCRIBE, "", 0);
    rc = zmq_connect (self->subscriber, endpoint);
    assert (rc == 0);

    snprintf (endpoint, 256, "%s:%d", address, atoi (service) + 2);
    self->publisher = zmq_socket (context, ZMQ_PUB);
    rc = zmq_connect (self->publisher, endpoint);
    assert (rc == 0);

    self->expires = s_clock () + SERVER_TTL;
    return self;
}

void
server_destroy (server_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        server_t *self = *self_p;
        int zero = 0;
        //  This machinery will be eliminated with libzap...
        zmq_setsockopt (self->snapshot,
            ZMQ_LINGER, &zero, sizeof (zero));
        zmq_setsockopt (self->publisher,
            ZMQ_LINGER, &zero, sizeof (zero));
        zmq_setsockopt (self->subscriber,
            ZMQ_LINGER, &zero, sizeof (zero));
        zmq_close (self->snapshot);
        zmq_close (self->publisher);
        zmq_close (self->subscriber);
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
    void *context;              //  0MQ context
    void *control;              //  Socket to talk to application
    zhash_t *kvmap;             //  Actual key/value table
    server_t *server [SERVER_MAX];
    uint nbr_servers;           //  0 to SERVER_MAX
    uint state;                 //  Current state
    uint cur_server;            //  If active, server 0 or 1
    int64_t sequence;           //  Last kvmsg processed
} agent_t;

agent_t *
agent_new (void *context, char *endpoint)
{
    agent_t *self = (agent_t *) calloc (1, sizeof (agent_t));
    self->context = context;
    self->control = zmq_socket (self->context, ZMQ_PAIR);
    assert (self->control);
    self->kvmap = zhash_new ();
    self->state = STATE_STARTUP;
    int rc = zmq_connect (self->control, endpoint);
    assert (rc == 0);
    return self;
}

void
agent_destroy (agent_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        agent_t *self = *self_p;
        int zero = 0;
        zmq_setsockopt (self->control,
            ZMQ_LINGER, &zero, sizeof (zero));
        zmq_close (self->control);
        server_destroy (&self->server [0]);
        server_destroy (&self->server [1]);
        zhash_destroy (&self->kvmap);
        free (self);
        *self_p = NULL;
    }
}

//  Returns -1 if thread was interrupted
int
agent_control_message (agent_t *self)
{
    zmsg_t *msg = zmsg_recv (self->control);
    char *command = zmsg_pop (msg);
    if (command == NULL)
        return -1;

    if (streq (command, "CONNECT")) {
        char *address = zmsg_pop (msg);
        char *service = zmsg_pop (msg);
        if (self->nbr_servers < SERVER_MAX)
            self->server [self->nbr_servers++]
                = server_new (self->context, address, service);
        else
            printf ("E: too many servers (max. %d)\n", SERVER_MAX);
        free (address);
        free (service);
    }
    else
    if (streq (command, "SET")) {
        char *key = zmsg_pop (msg);
        //  We're doing string values for now, will switch to frames
        //  when we move this to libzapi.
        char *value = zmsg_pop (msg);
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
        char *key = zmsg_pop (msg);
        char *value = zhash_lookup (self->kvmap, key);
        if (value)
            s_send (self->control, value);
        else
            s_send (self->control, "");
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

static void *
clone_agent (void *context)
{
    agent_t *self = agent_new (context, "inproc://clone");

    while (!s_interrupted) {
        zmq_pollitem_t items [] = {
            { self->control, 0, ZMQ_POLLIN, 0 },
            { 0,             0, ZMQ_POLLIN, 0 }
        };
        server_t *server = self->server [self->cur_server];
        switch (self->state) {
            case STATE_STARTUP:
                if (self->nbr_servers > 0) {
                    self->state = STATE_PENDING;
                    server->expires = s_clock () + SERVER_TTL;
                    s_send (server->snapshot, "I can haz state?");
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
        int tickless = (int) (expires - s_clock ());
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
            server->expires = s_clock () + SERVER_TTL;
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
        if (s_clock () >= expires) {
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
    return NULL;
}
