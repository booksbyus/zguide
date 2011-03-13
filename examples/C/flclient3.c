//
//  Freelance client - Model 3
//  Uses ROUTER socket to address specific services
//  IN PROGRESS, not complete
//
#include "zmsg.h"
#include "zhash.h"
#include "zlist.h"

//  If no server replies within this time, abandon request
#define GLOBAL_TIMEOUT  3000    //  msecs
//  Send keep-alive PINGs to all servers every this often
#define PING_INTERVAL  5000    //  msecs
//  If server doesn't show signs of life after this, assume dead
#define PING_TIMEOUT    10000   //  msecs

//  We design our client API as a class

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _flclient_t flclient_t;

flclient_t *flclient_new     (void);
void        flclient_destroy (flclient_t **self_p);
void        flclient_connect (flclient_t *self, char *endpoint);
zmsg_t *    flclient_request (flclient_t *self, zmsg_t **request_p);

#ifdef __cplusplus
}
#endif


int main (void)
{
    //  Create new freelance client object
    flclient_t *client = flclient_new ();
    
    //  Connect to several endpoints
    flclient_connect (client, "tcp://localhost:5555");
    flclient_connect (client, "tcp://localhost:5556");
    flclient_connect (client, "tcp://localhost:5557");
    //  Allow connections to come up
    s_sleep (100);
    
    //  Send a bunch of name resolution 'requests', measure time
    int requests = 10000;
    uint64_t start = s_clock ();
    while (requests--) {
        zmsg_t *request = zmsg_new ("random name");
        zmsg_t *reply = flclient_request (client, &request);
        if (!reply) {
            printf ("E: name service not available, aborting\n");
            break;
        }
        zmsg_destroy (&reply);
    }
    printf ("Average round trip cost: %d usec\n", 
        (int) (s_clock () - start) / 10);
    
    flclient_destroy (&client);
    return 0;
}


//  ====================================================================
//  Synchronous part, works in our application thread

//  --------------------------------------------------------------------
//  Structure of our class

struct _flclient_t {
    void *context;      //  Our 0MQ context
    void *control;      //  Inproc socket talking to flclient task
};

static void *flclient_task (void *context);

//  --------------------------------------------------------------------
//  Constructor

flclient_t *
flclient_new (void)
{
    flclient_t
        *self;

    s_catch_signals ();
    self = (flclient_t *) malloc (sizeof (flclient_t));
    self->context = zmq_init (1);
    self->control = zmq_socket (self->context, ZMQ_PAIR);

    int rc = zmq_bind (self->control, "inproc://flclient");
    assert (rc == 0);
    
    pthread_t thread;
    pthread_create (&thread, NULL, flclient_task, self->context);
    pthread_detach (thread);
    
    return self;
}

//  --------------------------------------------------------------------
//  Destructor

void
flclient_destroy (flclient_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        flclient_t *self = *self_p;

        puts ("TERMINATION");
        zmq_close (self->control);
        zmq_term (self->context);

        //  Free object structure
        free (self);
        *self_p = NULL;
    }
}

//  --------------------------------------------------------------------
//  Connect to new server endpoint

void
flclient_connect (flclient_t *self, char *endpoint)
{
    assert (self);
    assert (endpoint);
    
    zmsg_t *msg = zmsg_new (endpoint);
    zmsg_push (msg, "CONNECT");
    zmsg_send (&msg, self->control);
}

//  --------------------------------------------------------------------
//  Send & destroy request, get reply

zmsg_t *
flclient_request (flclient_t *self, zmsg_t **request_p)
{
    assert (self);
    assert (*request_p);
    
    zmsg_push (*request_p, "REQUEST");
    zmsg_send (request_p, self->control);
    zmsg_t *reply = zmsg_recv (self->control);
//puts ("FLCLIENT REQUEST:");
//zmsg_dump (reply);
    return reply;
}


//  ====================================================================
//  Asynchronous part, works in the background

//  --------------------------------------------------------------------
//  Simple class for one server we talk to

typedef struct {
    char *endpoint;             //  Server identity/endpoint
    int alive;                  //  1 if known to be alive
    int64_t expiry;             //  Time to live
} server_t;

server_t *
server_new (char *endpoint)
{
    server_t *self = (server_t *) malloc (sizeof (server_t));
    self->endpoint = strdup (endpoint); 
    self->alive = 1;            //  Assume alive at startup
    self->expiry = s_clock () + PING_TIMEOUT;
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

int server_ping (char *endpoint, void *server, void *socket)
{
    puts ("PING SERVER");
    zmsg_t *ping = zmsg_new ("PING");
    zmsg_push (ping, endpoint);
zmsg_dump (ping);
    zmsg_send (&ping, socket);
    return 0;
}


//  --------------------------------------------------------------------
//  Simple class for one background agent

typedef struct {
    void *context;              //  0MQ context
    zhash_t *servers;           //  Servers we've connected to
    zlist_t *actives;           //  Servers we know are alive
    uint64_t ping_at;           //  When to send PING messages
    uint sequence;              //  Number of requests ever sent
    void *control;              //  Socket to talk to application
    void *router;               //  Socket to talk to servers
    zmsg_t *request;            //  Current request if any
    zmsg_t *reply;              //  Current reply if any
    int64_t expiry;             //  Timeout for request/reply
} agent_t;

agent_t *
agent_new (void *context, char *endpoint)
{
    agent_t *self = (agent_t *) calloc (1, sizeof (agent_t));
    self->context = context;
    self->servers = zhash_new ();
    self->actives = zlist_new ();
    self->control = zmq_socket (self->context, ZMQ_PAIR);
    self->router  = zmq_socket (self->context, ZMQ_XREP);
    self->ping_at = s_clock () + PING_INTERVAL;
    zmq_connect (self->control, endpoint);
    return self;
}

void
agent_destroy (agent_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        agent_t *self = *self_p;
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
    
    if (strcmp (command, "CONNECT") == 0) {
        char *endpoint = zmsg_pop (msg);
        printf ("I: connecting to %s...\n", endpoint);
        int rc = zmq_connect (self->router, endpoint);
        assert (rc == 0);
        server_t *server = server_new (endpoint);
        zhash_insert (self->servers, endpoint, (void *) server);
        zhash_freefn (self->servers, endpoint, s_server_free);
        free (endpoint);
        //  When topology changes, ping all servers quickly
        self->ping_at = s_clock () + 100;
    }
    else
    if (strcmp (command, "REQUEST") == 0) {
        assert (!self->request);    //  Strict request-reply cycle
        //  Prefix request with sequence number and empty envelope
        char sequence_text [10];
        sprintf (sequence_text, "%u", ++self->sequence);
        zmsg_push (msg, sequence_text);
        //  Take ownership of request message
        self->request = msg;
        msg = NULL;
        //  Request expires after global timeout
        self->expiry = s_clock () + GLOBAL_TIMEOUT;
    }
    free (command);
    zmsg_destroy (&msg);
}

void 
agent_router_message (agent_t *self)
{
    zmsg_t *reply = zmsg_recv (self->router);
zmsg_dump (reply); //xx
    
    //  Frame 0 is server that replied
    char *endpoint = zmsg_pop (reply);
    server_t *server = (server_t *) zhash_lookup (self->servers, endpoint);
    assert (server);
    if (!server->alive) {
        zlist_append (self->actives, server);
        server->alive = 1;
    }
    server->expiry = s_clock () + PING_TIMEOUT;
    free (endpoint);
    
    //  Frame 1 may be sequence number for reply
    if (zmsg_parts (reply) > 1
    &&  atoi (zmsg_address (reply)) == self->sequence) {
        free (zmsg_pop (reply));
        zmsg_send (&reply, self->control);
        zmsg_destroy (&self->request);
    }
    zmsg_destroy (&reply);
}


//  --------------------------------------------------------------------
//  Asynchronous agent manages server pool and handles request/reply
//  dialog when the application asks for it.

static void *
flclient_task (void *context) 
{
    agent_t *self = agent_new (context, "inproc://flclient");
    zmq_pollitem_t items [] = { 
        { self->control, 0, ZMQ_POLLIN, 0 },
        { self->router, 0, ZMQ_POLLIN, 0 } 
    };
    
    while (!s_interrupted) {
        //  Calculate tickless timer
        uint64_t tickless = self->ping_at;
        if (tickless > self->expiry)
            tickless = self->expiry;
        
    puts ("POLL");
        int rc = zmq_poll (items, 2, (tickless - s_clock ()) * 1000);
        if (rc == -1 && errno == ETERM)
            break;              //  Context has been shut down
        
        if (items [0].revents & ZMQ_POLLIN)
            agent_control_message (self);
        
        if (items [1].revents & ZMQ_POLLIN)
            agent_router_message (self);
        
        //  If we're processing a request, dispatch to next server
        if (self->request) {
            if (s_clock () >= self->expiry) {
                //  Request expired, kill it
                zmsg_t *reply = zmsg_new ("FAILED");
                zmsg_send (&reply, self->control);
                zmsg_destroy (&self->request);
            }
            else {
            // - expire dead servers as we progress
            // if (server->expiry < s_clock ())
            //  server->alive = 0;
                server_t *server = (server_t *) zlist_first (self->actives);
                if (server) {
                    zmsg_t *request = zmsg_dup (self->request);
                    zmsg_push (request, server->endpoint);
                    zmsg_send (&request, self->router);
                }
            }
        }
        //  Disconnect and delete any expired servers
        //  Send heartbeats to idle servers if needed
        if (s_clock () >= self->ping_at) {
            zhash_apply (self->servers, server_ping, self->router);
            self->ping_at = s_clock () + PING_INTERVAL;
        }
    }
puts ("EXITING");
    agent_destroy (&self);
    return NULL;
}





#define PING_INTERVAL  5000    //  msecs


//  If server doesn't show signs of life after this, assume dead
#define PING_TIMEOUT    10000   //  msecs
