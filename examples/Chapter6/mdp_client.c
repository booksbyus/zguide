/*  =========================================================================
    mdp_client.c

    Generated codec implementation for mdp_client
    =========================================================================
*/

#include <czmq.h>
#include "mdp_client.h"

//  Structure of our class

struct _mdp_client_t {
    int id;                     //  mdp_client message ID
    zframe_t *address;          //  Sender address if any
    char *service;
    zframe_t *body;
};


//  --------------------------------------------------------------------------
//  Create a new mdp_client

mdp_client_t *
mdp_client_new (void)
{
    mdp_client_t *self = (mdp_client_t *) zmalloc (sizeof (mdp_client_t));
    return self;
}


//  --------------------------------------------------------------------------
//  Destroy the mdp_client

void
mdp_client_destroy (mdp_client_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        mdp_client_t *self = *self_p;

        //  Free class properties
        zframe_destroy (&self->address);
        free (self->service);
        zframe_destroy (&self->body);
        //  Free object itself
        free (self);
        *self_p = NULL;
    }
}


//  --------------------------------------------------------------------------
//  Receive and parse a mdp_client from the socket. Returns new object or
//  NULL if error. Will block if there's no message waiting.

mdp_client_t *
mdp_client_recv (void *socket)
{
    //  Read all frames off socket
    assert (socket);
    zmsg_t *msg = zmsg_recv (socket);
    if (!msg)
        return NULL;            //  Interrupted

    //  Create and populate new mdp_client instance
    mdp_client_t *self = mdp_client_new ();
    
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
    if (!protocol_frame || !zframe_streq (protocol_frame, "MDPC01"))
        goto malformed;
    zframe_destroy (&protocol_frame);

    //  All messages have the same structure
    self->service = zmsg_popstr (msg);
    if (!self->service)
        goto malformed;
    self->body = zmsg_pop (msg);
    if (!self->body)
        goto malformed;

    //  Successful return
    zmsg_destroy (&msg);
    return self;

    //  Error returns
    malformed:
        printf ("E: malformed message '%d'\n", self->id);
    empty:
        zmsg_destroy (&msg);
        mdp_client_destroy (&self);
        return (NULL);
}


//  --------------------------------------------------------------------------
//  Send the mdp_client to the socket, and destroy it

int
mdp_client_send (mdp_client_t **self_p, void *socket)
{
    assert (socket);
    assert (self_p);
    assert (*self_p);
    mdp_client_t *self = *self_p;

    //  If we're sending to a ROUTER, we send the address first
    zmsg_t *msg = zmsg_new ();
    if (zsockopt_type (socket) == ZMQ_ROUTER) {
        assert (self->address);
        zmsg_add (msg, self->address);
        self->address = NULL;       //  Owned by msg now
    }
    //  Send header fields
    zmsg_addstr (msg, "");
    zmsg_addstr (msg, "MDPC01");

    //  All messages have the same structure
    zmsg_addstr (msg, self->service);
    zmsg_add (msg, self->body);
    self->body = NULL;

    //  Send the message and destroy mdp_client object
    int rc = zmsg_send (&msg, socket);
    mdp_client_destroy (self_p);
    return rc;
}


//  --------------------------------------------------------------------------
//  Get/set the message address

zframe_t *
mdp_client_address (mdp_client_t *self)
{
    assert (self);
    return self->address;
}

void
mdp_client_address_set (mdp_client_t *self, zframe_t *address)
{
    if (self->address)
        zframe_destroy (&self->address);
    self->address = zframe_dup (address);
}


//  --------------------------------------------------------------------------
//  Get/set the service field

char *
mdp_client_service (mdp_client_t *self)
{
    assert (self);
    return self->service;
}

void
mdp_client_service_set (mdp_client_t *self, char *format, ...)
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
//  Get/set the body field

zframe_t *
mdp_client_body (mdp_client_t *self)
{
    assert (self);
    return self->body;
}

//  Takes ownership of supplied frame
void
mdp_client_body_set (mdp_client_t *self, zframe_t *frame)
{
    assert (self);
    if (self->body)
        zframe_destroy (&self->body);
    self->body = frame;
}


//  --------------------------------------------------------------------------
//  Selftest

int
mdp_client_test (bool verbose)
{
    printf (" * mdp_client: ");

    //  Simple create/destroy test
    mdp_client_t *self = mdp_client_new ();
    assert (self);
    mdp_client_destroy (&self);

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
    self = mdp_client_new ();
    mdp_client_service_set (self, "Life is short but Now lasts for ever");
    mdp_client_body_set (self, zframe_new ("Captcha Diem", 12));
    mdp_client_send (&self, output);

    self = mdp_client_recv (input);
    assert (self);
    assert (streq (mdp_client_service (self), "Life is short but Now lasts for ever"));
    assert (zframe_streq (mdp_client_body (self), "Captcha Diem"));
    mdp_client_destroy (&self);

    zctx_destroy (&ctx);
    printf ("OK\n");
    return 0;
}
