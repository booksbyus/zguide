/*  =========================================================================
    nom_server.c

    Generated class for nom_server protocol server
    =========================================================================
*/

#include <czmq.h>
#include "nom_server.h"

//  This API works in two halves so that the main server engine can run
//  in the background. One half is a front-end object the caller creates
//  and works with; the other half is a back-end "agent" that runs in a
//  background thread. The front-end talks to the back-end over an inproc
//  pipe socket.

//  ---------------------------------------------------------------------
//  Structure of our front-end class

struct _nom_server_t {
    zctx_t *ctx;        //  CZMQ context
    void *pipe;         //  Pipe through to agent
};

//  This is the thread that handles our real server class
static void
    server_agent (void *args, zctx_t *ctx, void *pipe);
    
//  --------------------------------------------------------------------------
//  Create a new nom_server

nom_server_t *
nom_server_new (void)
{
    nom_server_t *self = (nom_server_t *) zmalloc (sizeof (nom_server_t));
    self->ctx = zctx_new ();
    self->pipe = zthread_fork (self->ctx, server_agent, NULL);
    return self;
}


//  --------------------------------------------------------------------------
//  Destroy the nom_server

void
nom_server_destroy (nom_server_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        nom_server_t *self = *self_p;
        zstr_send (self->pipe, "STOP");
        char *string = zstr_recv (self->pipe);
        free (string);
        zctx_destroy (&self->ctx);
        free (self);
        *self_p = NULL;
    }
}


//  --------------------------------------------------------------------------
//  Bind the nom_server to an endpoint

void
nom_server_bind (nom_server_t *self, const char *endpoint)
{
    assert (self);
    assert (endpoint);
    zstr_sendm (self->pipe, "BIND");
    zstr_send  (self->pipe, endpoint);
}


//  --------------------------------------------------------------------------
//  Execute the nom_server until interrupted

void
nom_server_wait (nom_server_t *self)
{
    while (!zctx_interrupted)
        sleep (1);
}


//  ---------------------------------------------------------------------
//  State machine constants

typedef enum {
    stopped_state = 0,
    start_state = 1,
    authenticated_state = 2,
    ready_state = 3
} state_t;

typedef enum {
    ohai_event = 1,
    ok_event = 2,
    error_event = 3,
    icanhaz_event = 4,
    hugz_event = 5,
    heartbeat_event = 6
} event_t;

//  Names for animation
char *s_state_name [] = {
    "stopped",
    "start",
    "authenticated",
    "ready"
};

char *s_event_name [] = {
    "",
    "OHAI",
    "ok",
    "error",
    "ICANHAZ",
    "HUGZ",
    "heartbeat"
};


//  ---------------------------------------------------------------------
//  Simple class for one client we talk to

typedef struct {
    //  Properties accessible to state machine actions
    int64_t heartbeat;          //  Heartbeat interval
    event_t next_event;         //  Next event
    
    //  Properties you should NOT touch
    void *router;               //  Socket to client
    int64_t heartbeat_at;       //  Next heartbeat at this time
    int64_t expires;            //  Expires at this time
    state_t state;              //  Current state
    event_t event;              //  Current event
    char *hashkey;              //  Key into clients hash
    zframe_t *address;          //  Address frame
    zmsg_t *request;            //  Last received request
    zmsg_t *reply;              //  Reply to send out, if any
} client_t;

static client_t *
client_new (void *router, char *hashkey, zframe_t *address)
{
    client_t *self = (client_t *) zmalloc (sizeof (client_t));
    self->heartbeat = 1000;
    self->router = router;
    self->hashkey = hashkey;
    self->address = address;
    self->state = start_state;
    self->reply = zmsg_new ();
    zmsg_add (self->reply, zframe_dup (self->address));
    return self;
}

static void
client_destroy (client_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        client_t *self = *self_p;
        zframe_destroy (&self->address);
        zmsg_destroy (&self->request);
        zmsg_destroy (&self->reply);
        free (self->hashkey);
        free (self);
        *self_p = NULL;
    }
}

static void
check_credentials_action (client_t *self) {
    char *body = zmsg_popstr (self->request);
    if (body && streq (body, "Joe"))
        self->next_event = ok_event;
    else
        self->next_event = error_event;
    free (body);
}

//  Execute state machine as long as we have events

static void
client_execute (client_t *self, int event)
{
    self->next_event = event;
    while (self->next_event) {
        self->event = self->next_event;
        self->next_event = 0;
        printf ("State=%s, event=%s\n",
            s_state_name [self->state], s_event_name [self->event]);
        switch (self->state) {
            case start_state:
                if (self->event == ohai_event) {
                check_credentials_action (self);
                    self->state = authenticated_state;
                }
                break;
                
            case authenticated_state:
                if (self->event == ok_event) {
                    zmsg_addstr (self->reply, "OHAI-OK");
                    self->state = ready_state;
                }
                else
                if (self->event == error_event) {
                    zmsg_addstr (self->reply, "WTF");
                    self->state = start_state;
                }
                break;
                
            case ready_state:
                if (self->event == icanhaz_event) {
                    zmsg_addstr (self->reply, "CHEEZBURGER");
                }
                else
                if (self->event == hugz_event) {
                    zmsg_addstr (self->reply, "HUGZ-OK");
                }
                else
                if (self->event == heartbeat_event) {
                    zmsg_addstr (self->reply, "HUGZ");
                }
                break;
                
            case stopped_state:
                //  Discard all events silently
                break;
        }
        if (zmsg_size (self->reply) > 1) {
            puts ("Send message to client");
            zmsg_dump (self->reply);
            zmsg_send (&self->reply, self->router);
            self->reply = zmsg_new ();
            zmsg_add (self->reply, zframe_dup (self->address));
        }
    }
}

static void
client_set_request (client_t *self, zmsg_t *request)
{
    if (self->request)
        zmsg_destroy (&self->request);
    self->request = request;

    //  Any input from client counts as activity
    self->heartbeat_at = zclock_time () + self->heartbeat;
    self->expires = zclock_time () + self->heartbeat * 3;
}

//  Client hash function that checks if client is alive
static int
client_ping (const char *key, void *client, void *socket)
{
    client_t *self = (client_t *) client;
    if (zclock_time () >= self->heartbeat_at) {
        client_execute (self, heartbeat_event);
        self->heartbeat_at = zclock_time () + self->heartbeat;
    }
    return 0;
}

//  Client hash function that calculates tickless timer
static int
client_tickless (const char *key, void *client, void *arg)
{
    client_t *self = (client_t *) client;
    uint64_t *tickless = (uint64_t *) arg;
    if (*tickless > self->heartbeat_at)
        *tickless = self->heartbeat_at;
    return 0;
}

//  Callback when we remove client from 'clients' hash table
static void
client_free (void *argument)
{
    client_t *client = (client_t *) argument;
    client_destroy (&client);
}


//  ---------------------------------------------------------------------
//  Context for the server agent

typedef struct {
    zctx_t *ctx;                //  Own CZMQ context
    void *pipe;                 //  Socket to back to caller
    void *router;               //  Socket to talk to clients
    zhash_t *clients;           //  Clients we've connected to
    bool stopped;               //  Has agent stopped?
} agent_t;

static agent_t *
agent_new (zctx_t *ctx, void *pipe)
{
    agent_t *self = (agent_t *) zmalloc (sizeof (agent_t));
    self->ctx = ctx;
    self->pipe = pipe;
    self->router = zsocket_new (self->ctx, ZMQ_ROUTER);
    self->clients = zhash_new ();
    return self;
}

static void
agent_destroy (agent_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        agent_t *self = *self_p;
        zhash_destroy (&self->clients);
        free (self);
        *self_p = NULL;
    }
}

static void
agent_control_message (agent_t *self)
{
    zmsg_t *msg = zmsg_recv (self->pipe);
    char *method = zmsg_popstr (msg);
    if (streq (method, "BIND")) {
        char *endpoint = zmsg_popstr (msg);
        assert (endpoint);
        int rc = zmq_bind (self->router, endpoint);
        assert (rc == 0);
        free (endpoint);
    }
    else
    if (streq (method, "STOP")) {
        zstr_send (self->pipe, "OK");
        self->stopped = true;
    }
    free (method);
    zmsg_destroy (&msg);
}

static void
agent_client_message (agent_t *self)
{
    zmsg_t *msg = zmsg_recv (self->router);
    if (!msg)
        return;         //  Interrupted; do nothing

    puts ("Received message from client");
    zmsg_dump (msg);

    //  Frame 0 is address client that sent message
    zframe_t *address = zmsg_pop (msg);
    char *hashkey = zframe_strhex (address);
    client_t *client = zhash_lookup (self->clients, hashkey);
    if (client == NULL) {
        client = client_new (self->router, hashkey, address);
        zhash_insert (self->clients, hashkey, client);
        zhash_freefn (self->clients, hashkey, client_free);
    }
    else {
        free (hashkey);
        zframe_destroy (&address);
    }
    //  Frame 1 is the command
    char *command = zmsg_popstr (msg);
    client_set_request (client, msg);
    if (command) {
        if (streq (command, "OHAI"))
            client_execute (client, ohai_event);
        else
        if (streq (command, "ICANHAZ"))
            client_execute (client, icanhaz_event);
        else
        if (streq (command, "HUGZ"))
            client_execute (client, hugz_event);
        free (command);
    }
}

//  Finally here's the agent task itself, which polls its two sockets
//  and processes incoming messages:

static void
server_agent (void *args, zctx_t *ctx, void *pipe)
{
    agent_t *self = agent_new (ctx, pipe);
    zmq_pollitem_t items [] = {
        { self->pipe, 0, ZMQ_POLLIN, 0 },
        { self->router, 0, ZMQ_POLLIN, 0 }
    };
    while (!self->stopped) {
        //  Calculate tickless timer, up to 1 hour
        uint64_t tickless = zclock_time () + 1000 * 3600;
        zhash_foreach (self->clients, client_tickless, &tickless);

        //  Poll until at most next timer event
        int rc = zmq_poll (items, 2,
            (tickless - zclock_time ()) * ZMQ_POLL_MSEC);
        if (rc == -1)
            break;              //  Context has been shut down

        //  Process incoming message from either socket
        if (items [0].revents & ZMQ_POLLIN)
            agent_control_message (self);

        if (items [1].revents & ZMQ_POLLIN)
            agent_client_message (self);

        //  Send heartbeats to idle clients as needed
        zhash_foreach (self->clients, client_ping, self->router);
    }
    agent_destroy (&self);
}


//  --------------------------------------------------------------------------
//  Selftest

int
nom_server_test (bool verbose)
{
    printf (" * nom_server: \n");

    nom_server_t *self = nom_server_new ();
    assert (self);
    nom_server_bind (self, "tcp://*:6000");
    
    zctx_t *ctx = zctx_new ();
    void *dealer = zsocket_new (ctx, ZMQ_DEALER);
    zsocket_set_rcvtimeo (dealer, 2000);
    zsocket_connect (dealer, "tcp://localhost:6000");

    //  Run self-tests
    zmsg_t *msg;
    char *command;
    msg = zmsg_new ();
    zmsg_addstr (msg, "OHAI");
    zmsg_addstr (msg, "Sleepy");
    zmsg_send (&msg, dealer);
    
    msg = zmsg_recv (dealer);
    assert (msg);
    command = zmsg_popstr (msg);
    assert (streq (command, "WTF"));
    free (command);
    zmsg_destroy (&msg);
    
    msg = zmsg_new ();
    zmsg_addstr (msg, "OHAI");
    zmsg_addstr (msg, "Joe");
    zmsg_send (&msg, dealer);
    
    msg = zmsg_recv (dealer);
    assert (msg);
    command = zmsg_popstr (msg);
    assert (streq (command, "OHAI-OK"));
    free (command);
    zmsg_destroy (&msg);
    
    msg = zmsg_new ();
    zmsg_addstr (msg, "ICANHAZ");
    zmsg_send (&msg, dealer);
    
    msg = zmsg_recv (dealer);
    assert (msg);
    command = zmsg_popstr (msg);
    assert (streq (command, "CHEEZBURGER"));
    free (command);
    zmsg_destroy (&msg);
    
    msg = zmsg_new ();
    zmsg_addstr (msg, "HUGZ");
    zmsg_send (&msg, dealer);
    
    msg = zmsg_recv (dealer);
    assert (msg);
    command = zmsg_popstr (msg);
    assert (streq (command, "HUGZ-OK"));
    free (command);
    zmsg_destroy (&msg);
    
    msg = zmsg_recv (dealer);
    assert (msg);
    command = zmsg_popstr (msg);
    assert (streq (command, "HUGZ"));
    free (command);
    zmsg_destroy (&msg);
    
    zctx_destroy (&ctx);
    nom_server_destroy (&self);

    printf ("OK\n");
    return 0;
}
