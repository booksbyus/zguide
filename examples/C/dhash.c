/*  =====================================================================
    dhash - distributed hash

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

#include "dhash.h"

//  If no server replies within this time, abandon request
#define GLOBAL_TIMEOUT  3000    //  msecs
//  PING interval for servers we think are alive
#define PING_INTERVAL   2000    //  msecs
//  Server considered dead if silent for this long
#define SERVER_TTL      6000    //  msecs


//  =====================================================================
//  Synchronous part, works in our application thread

//  ---------------------------------------------------------------------
//  Structure of our class

struct _dhash_t {
    void *context;      //  Our 0MQ context
    void *control;      //  Inproc socket talking to dhash task
};

static void *dhash_agent (void *context);

//  ---------------------------------------------------------------------
//  Constructor

dhash_t *
dhash_new (void)
{
    dhash_t
        *self;

    s_catch_signals ();
    self = (dhash_t *) malloc (sizeof (dhash_t));
    self->context = zmq_init (1);
    self->control = zmq_socket (self->context, ZMQ_PAIR);
    int rc = zmq_bind (self->control, "inproc://dhash");
    assert (rc == 0);

    pthread_t thread;
    pthread_create (&thread, NULL, dhash_agent, self->context);
    pthread_detach (thread);

    return self;
}

//  ---------------------------------------------------------------------
//  Destructor

void
dhash_destroy (dhash_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        dhash_t *self = *self_p;
        zmq_close (self->control);
        zmq_term (self->context);
        free (self);
        *self_p = NULL;
    }
}

//  ---------------------------------------------------------------------
//  Connect to new server endpoint
//  Sends [CONNECT][endpoint] to the agent

void
dhash_connect (dhash_t *self, char *address, int port)
{
    assert (self);
    assert (endpoint);
    zmsg_t *msg = zmsg_new (address);
    zmsg_push (msg, "CONNECT");
    zmsg_send (&msg, self->control);
}

//  ---------------------------------------------------------------------
//  Set new value in distributed hash table
//  Sends [SET][key][value] to the agent

void
dhash_set (dhash_t *self, char *key, char *value)
{
    assert (self);
    assert (endpoint);
    zmsg_t *msg = zmsg_new (value);
    zmsg_push (msg, key);
    zmsg_push (msg, "SET");
    zmsg_send (&msg, self->control);
}

//  ---------------------------------------------------------------------
//  Lookup value in distributed hash table
//  Sends [GET][key][value] to the agent and waits for a value response
//  If there is no dhash available, will eventually return NULL.

char *
dhash_get (dhash_t *self, char *key)
{
    assert (self);
    assert (endpoint);
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
    char *endpoint;             //  Server identity/endpoint
    uint alive;                 //  1 if known to be alive
    int64_t ping_at;            //  Next ping at this time
    int64_t expires;            //  Expires at this time
} server_t;

server_t *
server_new (char *endpoint)
{
    server_t *self = (server_t *) malloc (sizeof (server_t));
    self->endpoint = strdup (endpoint);
    self->alive = 0;
    self->ping_at = s_clock () + PING_INTERVAL;
    self->expires = s_clock () + SERVER_TTL;
    return self;
}

void
server_destroy (server_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        server_t *self = *self_p;
        free (self->endpoint);
        free (self);
        *self_p = NULL;
    }
}

int
server_ping (char *key, void *server, void *socket)
{
    server_t *self = (server_t *) server;
    if (s_clock () >= self->ping_at) {
        zmsg_t *ping = zmsg_new ("PING");
        zmsg_push (ping, self->endpoint);
        zmsg_send (&ping, socket);
        self->ping_at = s_clock () + PING_INTERVAL;
    }
    return 0;
}

int
server_tickless (char *key, void *server, void *arg)
{
    server_t *self = (server_t *) server;
    uint64_t *tickless = (uint64_t *) arg;
    if (*tickless > self->ping_at)
        *tickless = self->ping_at;
    return 0;
}


//  ---------------------------------------------------------------------
//  Simple class for one background agent

- keep primary & secondary server, no more or less
- each server three sockets, three endpoints
- connect to port P (PUB), P+1 (ROUTER), P+2 (SUB)
- specify port separatel from address

typedef struct {
    void *context;              //  0MQ context
    zhash_t *kvmap;             //  Actual key/value table
    zhash_t *servers;           //  Servers we've connected to
    zlist_t *actives;           //  Servers we know are alive
    uint sequence;              //  Number of requests ever sent
    void *control;              //  Socket to talk to application
    void *router;               //  Socket to talk to servers
    zmsg_t *request;            //  Current request if any
    zmsg_t *reply;              //  Current reply if any
    int64_t expires;            //  Timeout for request/reply
} agent_t;

agent_t *
agent_new (void *context, char *endpoint)
{
    agent_t *self = (agent_t *) calloc (1, sizeof (agent_t));
    self->context = context;
    self->kvmap = zhash_new ();
    self->servers = zhash_new ();
    self->actives = zlist_new ();
    self->control = zmq_socket (self->context, ZMQ_PAIR);
    self->router  = zmq_socket (self->context, ZMQ_XREP);
    zmq_connect (self->control, endpoint);
    return self;
}

void
agent_destroy (agent_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        agent_t *self = *self_p;
        zhash_destroy (&self->kvmap);
        zhash_destroy (&self->servers);
        zlist_destroy (&self->actives);
        zmq_close (self->control);
        int zero = 0;
        zmq_setsockopt (self->router, ZMQ_LINGER, &zero, sizeof (zero));
        zmq_close (self->router);
        zmsg_destroy (&self->request);
        zmsg_destroy (&self->reply);
        free (self);
        *self_p = NULL;
    }
}

//  Callback when we remove server from agent 'servers' hash table

static void
s_server_free (void *argument)
{
    server_t *server = (server_t *) argument;
    server_destroy (&server);
}

void 
agent_control_message (agent_t *self)
{
    zmsg_t *msg = zmsg_recv (self->control);
    char *command = zmsg_pop (msg);

    if (streq (command, "CONNECT")) {
        char *endpoint = zmsg_pop (msg);
        printf ("I: connecting to %s...\n", endpoint);
        int rc = zmq_connect (self->router, endpoint);
        assert (rc == 0);
        server_t *server = server_new (endpoint);
        zhash_insert (self->servers, endpoint, server);
        zhash_freefn (self->servers, endpoint, s_server_free);
        zlist_append (self->actives, server);
        server->ping_at = s_clock () + PING_INTERVAL;
        server->expires = s_clock () + SERVER_TTL;
        free (endpoint);
    }
    else
    if (streq (command, "GET")) {
    }
    else
    if (streq (command, "SET")) {
    }
    else
    if (streq (command, "REQUEST")) {
        assert (!self->request);    //  Strict request-reply cycle
        //  Prefix request with sequence number and empty envelope
        char sequence_text [10];
        sprintf (sequence_text, "%u", ++self->sequence);
        zmsg_push (msg, sequence_text);
        //  Take ownership of request message
        self->request = msg;
        msg = NULL;
        //  Request expires after global timeout
        self->expires = s_clock () + GLOBAL_TIMEOUT;
    }
    free (command);
    zmsg_destroy (&msg);
}

void 
agent_router_message (agent_t *self)
{
    zmsg_t *reply = zmsg_recv (self->router);

    //  Frame 0 is server that replied
    char *endpoint = zmsg_pop (reply);
    server_t *server = 
        (server_t *) zhash_lookup (self->servers, endpoint);
    assert (server);
    free (endpoint);
    if (!server->alive) {
        zlist_append (self->actives, server);
        server->alive = 1;
    }
    server->ping_at = s_clock () + PING_INTERVAL;
    server->expires = s_clock () + SERVER_TTL;

    //  Frame 1 may be sequence number for reply
    if (zmsg_parts (reply) > 1
    &&  atoi (zmsg_address (reply)) == self->sequence) {
        free (zmsg_pop (reply));
        zmsg_push (reply, "OK");
        zmsg_send (&reply, self->control);
        zmsg_destroy (&self->request);
    }
    zmsg_destroy (&reply);
}


//  ---------------------------------------------------------------------
//  Asynchronous agent manages server pool and handles request/reply
//  dialog when the application asks for it.

static void *
dhash_agent (void *context)
{
    agent_t *self = agent_new (context, "inproc://dhash");
    zmq_pollitem_t items [] = { 
        { self->control, 0, ZMQ_POLLIN, 0 },
        { self->router, 0, ZMQ_POLLIN, 0 } 
    };




    //  Prepare our context and subscriber
    void *context = zmq_init (1);
    void *subscriber = zmq_socket (context, ZMQ_SUB);
    zmq_setsockopt (subscriber, ZMQ_SUBSCRIBE, "", 0);
    zmq_connect (subscriber, "tcp://localhost:5556");

    void *snapshot = zmq_socket (context, ZMQ_XREQ);
    zmq_connect (snapshot, "tcp://localhost:5557");

    void *updates = zmq_socket (context, ZMQ_PUSH);
    zmq_connect (updates, "tcp://localhost:5558");

    s_catch_signals ();
    srandom ((unsigned) time (NULL));

    //  Get state snapshot
    int64_t sequence = 0;
    s_send (snapshot, "I can haz state?");
    while (!s_interrupted) {
        kvmsg_t *kvmsg = kvmsg_recv (snapshot);
        if (!kvmsg)
            break;          //  Interrupted
        if (streq (kvmsg_key (kvmsg), "KTHXBAI")) {
            sequence = kvmsg_sequence (kvmsg);
            kvmsg_destroy (&kvmsg);
            break;          //  Done
        }
        kvmsg_store (&kvmsg, kvmap);
    }
    printf ("I: received snapshot=%" PRId64 "\n", sequence);
    int zero = 0;
    zmq_setsockopt (snapshot, ZMQ_LINGER, &zero, sizeof (zero));
    zmq_close (snapshot);

    int64_t alarm = s_clock () + 1000;
    while (!s_interrupted) {
        zmq_pollitem_t items [] = { { subscriber, 0, ZMQ_POLLIN, 0 } };
        int tickless = (int) ((alarm - s_clock ()));
        if (tickless < 0)
            tickless = 0;
        int rc = zmq_poll (items, 1, tickless * 1000);
        if (rc == -1)
            break;              //  Context has been shut down

        if (items [0].revents & ZMQ_POLLIN) {
            kvmsg_t *kvmsg = kvmsg_recv (subscriber);
            if (!kvmsg)
                break;          //  Interrupted

            //  Discard out-of-sequence kvmsgs, incl. heartbeats
            if (kvmsg_sequence (kvmsg) > sequence) {
                sequence = kvmsg_sequence (kvmsg);
                kvmsg_store (&kvmsg, kvmap);
                printf ("I: received update=%" PRId64 "\n", sequence);
            }
            else
                kvmsg_destroy (&kvmsg);
        }
        //  If we timed-out, generate a random kvmsg
        if (s_clock () >= alarm) {
            kvmsg_t *kvmsg = kvmsg_new (0);
            kvmsg_fmt_key  (kvmsg, "%d", randof (10000));
            kvmsg_fmt_body (kvmsg, "%d", randof (1000000));
            kvmsg_send (kvmsg, updates);
            kvmsg_destroy (&kvmsg);
            alarm = s_clock () + 1000;
        }
    }

    printf (" Interrupted\n%" PRId64 " messages in\n", sequence);
    zmq_setsockopt (updates, ZMQ_LINGER, &zero, sizeof (zero));
    zmq_close (updates);
    zmq_close (subscriber);
    zmq_term (context);
    return 0;


    
    while (!s_interrupted) {
        //  Calculate tickless timer, up to 1 hour
        uint64_t tickless = s_clock () + 1000 * 3600;
        if (self->request
        &&  tickless > self->expires)
            tickless = self->expires;
        zhash_foreach (self->servers, server_tickless, &tickless);

        int rc = zmq_poll (items, 2, (tickless - s_clock ()) * 1000);
        if (rc == -1 && errno == ETERM)
            break;              //  Context has been shut down

        if (items [0].revents & ZMQ_POLLIN)
            agent_control_message (self);

        if (items [1].revents & ZMQ_POLLIN)
            agent_router_message (self);

        //  If we're processing a request, dispatch to next server
        if (self->request) {
            if (s_clock () >= self->expires) {
                //  Request expired, kill it
                zmsg_t *reply = zmsg_new ("FAILED");
                zmsg_send (&reply, self->control);
                zmsg_destroy (&self->request);
            }
            else {
                //  Find server to talk to, remove any expired ones
                while (zlist_size (self->actives)) {
                    server_t *server = 
                        (server_t *) zlist_first (self->actives);
                    if (s_clock () >= server->expires) {
                        zlist_pop (self->actives);
                        server->alive = 0;
                    }
                    else {
                        zmsg_t *request = zmsg_dup (self->request);
                        zmsg_push (request, server->endpoint);
                        zmsg_send (&request, self->router);
                        break;
                    }
                }
            }
        }
        //  Disconnect and delete any expired servers
        //  Send heartbeats to idle servers if needed
        zhash_foreach (self->servers, server_ping, self->router);
    }
    agent_destroy (&self);
    return NULL;
}

