/*  =========================================================================
    mdp_worker.c

    Generated codec implementation for mdp_worker
    =========================================================================
*/

#include <czmq.h>
#include "mdp_worker.h"

//  Structure of our class

struct _mdp_worker_t {
    int id;                     //  mdp_worker message ID
    zframe_t *address;          //  Sender address if any
    char *service;
    zframe_t *client;
    zframe_t *body;
};


//  --------------------------------------------------------------------------
//  Create a new mdp_worker

mdp_worker_t *
mdp_worker_new (int id)
{
    mdp_worker_t *self = (mdp_worker_t *) zmalloc (sizeof (mdp_worker_t));
    self->id = id;
    return self;
}


//  --------------------------------------------------------------------------
//  Destroy the mdp_worker

void
mdp_worker_destroy (mdp_worker_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        mdp_worker_t *self = *self_p;

        //  Free class properties
        zframe_destroy (&self->address);
        free (self->service);
        zframe_destroy (&self->client);
        zframe_destroy (&self->body);
        //  Free object itself
        free (self);
        *self_p = NULL;
    }
}


//  --------------------------------------------------------------------------
//  Receive and parse a mdp_worker from the socket. Returns new object or
//  NULL if error. Will block if there's no message waiting.

mdp_worker_t *
mdp_worker_recv (void *socket)
{
    //  Read all frames off socket
    assert (socket);
    zmsg_t *msg = zmsg_recv (socket);
    if (!msg)
        return NULL;            //  Interrupted

    //  Create and populate new mdp_worker instance
    mdp_worker_t *self = mdp_worker_new (0);
    
    //  If we're reading from a ROUTER socket, get address
    if (zsockopt_type (socket) == ZMQ_ROUTER) {
        self->address = zmsg_pop (msg);
        if (!self->address)
            goto empty;         //  Interrupted
    }
    //  Read and check header
    zframe_t *empty_frame = zmsg_pop (msg);
    if (!empty_frame || !zframe_streq (empty_frame, ""))
        goto malformed;
    zframe_destroy (&empty_frame);

    zframe_t *protocol_frame = zmsg_pop (msg);
    if (!protocol_frame || !zframe_streq (protocol_frame, "MDPW01"))
        goto malformed;
    zframe_destroy (&protocol_frame);

    zframe_t *id_frame = zmsg_pop (msg);
    if (!id_frame)
        goto malformed;
    self->id = zframe_data (id_frame) [0];
    zframe_destroy (&id_frame);

    //  Read and parse fields per-message
    switch (self->id) {
        case MDP_WORKER_READY:
            self->service = zmsg_popstr (msg);
            if (!self->service)
                goto malformed;
            break;
            
        case MDP_WORKER_REQUEST:
            self->client = zmsg_pop (msg);
            if (!self->client)
                goto malformed;
            self->body = zmsg_pop (msg);
            if (!self->body)
                goto malformed;
            break;
            
        case MDP_WORKER_REPLY:
            self->client = zmsg_pop (msg);
            if (!self->client)
                goto malformed;
            self->body = zmsg_pop (msg);
            if (!self->body)
                goto malformed;
            break;
            
        case MDP_WORKER_HEARBEAT:
            break;
            
        case MDP_WORKER_DISCONNECT:
            break;
            
        default:
            goto malformed;
    }
    //  Successful return
    zmsg_destroy (&msg);
    return self;

    //  Error returns
    malformed:
        printf ("E: malformed message '%d'\n", self->id);
    empty:
        zmsg_destroy (&msg);
        mdp_worker_destroy (&self);
        return (NULL);
}


//  --------------------------------------------------------------------------
//  Send the mdp_worker to the socket, and destroy it

int
mdp_worker_send (mdp_worker_t **self_p, void *socket)
{
    assert (socket);
    assert (self_p);
    assert (*self_p);
    mdp_worker_t *self = *self_p;

    //  If we're sending to a ROUTER, we send the address first
    zmsg_t *msg = zmsg_new ();
    if (zsockopt_type (socket) == ZMQ_ROUTER) {
        assert (self->address);
        zmsg_add (msg, self->address);
        self->address = NULL;       //  Owned by msg now
    }
    //  Send header fields
    zmsg_addstr (msg, "");
    zmsg_addstr (msg, "MDPW01");
    zmsg_addmem (msg, &self->id, 1);

    switch (self->id) {
        case MDP_WORKER_READY:
            zmsg_addstr (msg, self->service);
            break;
        case MDP_WORKER_REQUEST:
            zmsg_add (msg, self->client);
            self->client = NULL;
            zmsg_add (msg, self->body);
            self->body = NULL;
            break;
        case MDP_WORKER_REPLY:
            zmsg_add (msg, self->client);
            self->client = NULL;
            zmsg_add (msg, self->body);
            self->body = NULL;
            break;
        case MDP_WORKER_HEARBEAT:
            break;
        case MDP_WORKER_DISCONNECT:
            break;
    }
    //  Send the message and destroy mdp_worker object
    int rc = zmsg_send (&msg, socket);
    mdp_worker_destroy (self_p);
    return rc;
}


//  --------------------------------------------------------------------------
//  Get/set the message address

zframe_t *
mdp_worker_address (mdp_worker_t *self)
{
    assert (self);
    return self->address;
}

void
mdp_worker_address_set (mdp_worker_t *self, zframe_t *address)
{
    if (self->address)
        zframe_destroy (&self->address);
    self->address = zframe_dup (address);
}


//  --------------------------------------------------------------------------
//  Get/set the mdp_worker id

int
mdp_worker_id (mdp_worker_t *self)
{
    assert (self);
    return self->id;
}

void
mdp_worker_id_set (mdp_worker_t *self, int id)
{
    self->id = id;
}


//  --------------------------------------------------------------------------
//  Get/set the service field

char *
mdp_worker_service (mdp_worker_t *self)
{
    assert (self);
    return self->service;
}

void
mdp_worker_service_set (mdp_worker_t *self, char *format, ...)
{
    //  Format into newly allocated string
    assert (self);
    va_list argptr;
    va_start (argptr, format);
    free (self->service);
    self->service = (char *) malloc (255 + 1);
    assert (self->service);
    vsnprintf (self->service, 255, format, argptr);
    va_end (argptr);
}


//  --------------------------------------------------------------------------
//  Get/set the client field

zframe_t *
mdp_worker_client (mdp_worker_t *self)
{
    assert (self);
    return self->client;
}

//  Takes ownership of supplied frame
void
mdp_worker_client_set (mdp_worker_t *self, zframe_t *frame)
{
    assert (self);
    if (self->client)
        zframe_destroy (&self->client);
    self->client = frame;
}

//  --------------------------------------------------------------------------
//  Get/set the body field

zframe_t *
mdp_worker_body (mdp_worker_t *self)
{
    assert (self);
    return self->body;
}

//  Takes ownership of supplied frame
void
mdp_worker_body_set (mdp_worker_t *self, zframe_t *frame)
{
    assert (self);
    if (self->body)
        zframe_destroy (&self->body);
    self->body = frame;
}


//  --------------------------------------------------------------------------
//  Selftest

int
mdp_worker_test (bool verbose)
{
    printf (" * mdp_worker: ");

    //  Simple create/destroy test
    mdp_worker_t *self = mdp_worker_new (0);
    assert (self);
    mdp_worker_destroy (&self);

    //  Create pair of sockets we can send through
    zctx_t *ctx = zctx_new ();
    assert (ctx);

    void *output = zsocket_new (ctx, ZMQ_DEALER);
    assert (output);
    zsocket_bind (output, "inproc://selftest");
    void *input = zsocket_new (ctx, ZMQ_ROUTER);
    assert (input);
    zsocket_connect (input, "inproc://selftest");

    //  Encode/send/decode and verify each message type

    self = mdp_worker_new (MDP_WORKER_READY);
    mdp_worker_service_set (self, "Life is short but Now lasts for ever");
    mdp_worker_send (&self, output);

    self = mdp_worker_recv (input);
    assert (self);
    assert (streq (mdp_worker_service (self), "Life is short but Now lasts for ever"));
    mdp_worker_destroy (&self);

    self = mdp_worker_new (MDP_WORKER_REQUEST);
    mdp_worker_client_set (self, zframe_new ("Captcha Diem", 12));
    mdp_worker_body_set (self, zframe_new ("Captcha Diem", 12));
    mdp_worker_send (&self, output);

    self = mdp_worker_recv (input);
    assert (self);
    assert (zframe_streq (mdp_worker_client (self), "Captcha Diem"));
    assert (zframe_streq (mdp_worker_body (self), "Captcha Diem"));
    mdp_worker_destroy (&self);

    self = mdp_worker_new (MDP_WORKER_REPLY);
    mdp_worker_client_set (self, zframe_new ("Captcha Diem", 12));
    mdp_worker_body_set (self, zframe_new ("Captcha Diem", 12));
    mdp_worker_send (&self, output);

    self = mdp_worker_recv (input);
    assert (self);
    assert (zframe_streq (mdp_worker_client (self), "Captcha Diem"));
    assert (zframe_streq (mdp_worker_body (self), "Captcha Diem"));
    mdp_worker_destroy (&self);

    self = mdp_worker_new (MDP_WORKER_HEARBEAT);
    mdp_worker_send (&self, output);

    self = mdp_worker_recv (input);
    assert (self);
    mdp_worker_destroy (&self);

    self = mdp_worker_new (MDP_WORKER_DISCONNECT);
    mdp_worker_send (&self, output);

    self = mdp_worker_recv (input);
    assert (self);
    mdp_worker_destroy (&self);

    zctx_destroy (&ctx);
    printf ("OK\n");
    return 0;
}
