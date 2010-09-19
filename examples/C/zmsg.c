/*  =========================================================================
    zmsg.h

    Multipart message class for example applications. Requires zhelpers.h.

    Follows the ZFL class conventions and is further developed as the ZFL
    zfl_msg class.  See http://zfl.zeromq.org for more details.

    -------------------------------------------------------------------------
    Copyright (c) 1991-2010 iMatix Corporation <www.imatix.com>
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

#ifndef __ZMSG_H_INCLUDED__
#define __ZMSG_H_INCLUDED__

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _zmsg_t zmsg_t;

//  Constructor and destructor
zmsg_t *zmsg_new      (void);
void    zmsg_destroy  (zmsg_t **self_p);

//  Receive and send message, wrapping new/destroy
zmsg_t *zmsg_recv     (void *socket);
void    zmsg_send     (zmsg_t **self, void *socket);

//  Report size of message
size_t  zmsg_parts    (zmsg_t *self);

//  Read and set message body part
char   *zmsg_body     (zmsg_t *self);
void    zmsg_set_body (zmsg_t *self, char *body);

//  Generic push/pop message part off front
void    zmsg_push     (zmsg_t *self, char *part);
char   *zmsg_pop      (zmsg_t *self);

//  Read and set message envelopes
char   *zmsg_address  (zmsg_t *self);
void    zmsg_wrap     (zmsg_t *self, char *address, char *delim);
char   *zmsg_unwrap   (zmsg_t *self);

//  Selftest for the class
int     zmsg_test     (int verbose);

#ifdef __cplusplus
}
#endif

#endif

//  Pretty arbitrary limit on complexity of a message
#define ZMSG_MAX_PARTS  255

//  Structure of our class
//  We access these properties only via class methods

struct _zmsg_t {
    //  Part data follows message recv/send order
    char *_part_data [ZMSG_MAX_PARTS];
    size_t _part_count;
};


//  --------------------------------------------------------------------------
//  Constructor

zmsg_t *
zmsg_new (void)
{
    zmsg_t
        *self;

    self = malloc (sizeof (zmsg_t));
    memset (self, 0, sizeof (zmsg_t));
    return (self);
}


//  --------------------------------------------------------------------------
//  Destructor

void
zmsg_destroy (zmsg_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        zmsg_t *self = *self_p;

        //  Free message parts, if any
        while (self->_part_count)
            free (zmsg_pop (self));

        //  Free object structure
        free (self);
        *self_p = NULL;
    }
}


//  --------------------------------------------------------------------------
//  Receive message from socket
//  Creates a new message and returns it
//  Blocks on recv if socket is not ready for input

zmsg_t *
zmsg_recv (void *socket)
{
    assert (socket);

    zmsg_t *self = zmsg_new ();
    while (1) {
        self->_part_data [self->_part_count++] = s_recv (socket);
        int64_t more;
        size_t more_size = sizeof (more);
        zmq_getsockopt (socket, ZMQ_RCVMORE, &more, &more_size);
        if (!more)
            break;      //  Last message part
    }
    return (self);
}


//  --------------------------------------------------------------------------
//  Send message to socket
//  Destroys message after sending

void zmsg_send (zmsg_t **self_p, void *socket)
{
    assert (self_p);
    assert (*self_p);
    assert (socket);
    zmsg_t *self = *self_p;

    int part_nbr;
    for (part_nbr = 0; part_nbr < self->_part_count - 1; part_nbr++)
        s_sendmore (socket, self->_part_data [part_nbr]);
    s_send (socket, self->_part_data [part_nbr]);
    zmsg_destroy (self_p);
}


//  --------------------------------------------------------------------------
//  Report size of message

size_t
zmsg_parts (zmsg_t *self)
{
    return (self->_part_count);
}


//  --------------------------------------------------------------------------
//  Return pointer to message body, if any
//  Caller should not modify the provided data

char *
zmsg_body (zmsg_t *self)
{
    assert (self);

    if (self->_part_count)
        return (self->_part_data [self->_part_count - 1]);
    else
        return (NULL);
}


//  --------------------------------------------------------------------------
//  Set message body as copy of provided string
//  If message is empty, creates a new message body

void zmsg_set_body (zmsg_t *self, char *body)
{
    assert (self);
    assert (body);

    if (self->_part_count) {
        assert (self->_part_data [self->_part_count - 1]);
        free (self->_part_data [self->_part_count - 1]);
    }
    else
        self->_part_count = 1;

    self->_part_data [self->_part_count - 1] = strdup (body);
}


//  --------------------------------------------------------------------------
//  Push message part to front of message parts

void zmsg_push (zmsg_t *self, char *part)
{
    assert (self);
    assert (part);
    assert (self->_part_count < ZMSG_MAX_PARTS - 1);

    //  Move part stack up one element and insert new part
    memmove (
        &self->_part_data [1],
        &self->_part_data [0],
        sizeof (char *) * (ZMSG_MAX_PARTS - 1));
    self->_part_data [0] = strdup (part);
    self->_part_count += 1;
}


//  --------------------------------------------------------------------------
//  Pop message part off front of message parts
//  Caller should free returned string when finished with it

char *zmsg_pop (zmsg_t *self)
{
    assert (self);
    assert (self->_part_count);

    //  Remove first part and move part stack down one element
    char *part = self->_part_data [0];
    memmove (
        &self->_part_data [0],
        &self->_part_data [1],
        sizeof (char *) * (ZMSG_MAX_PARTS - 1));
    self->_part_count--;
    return (part);
}


//  --------------------------------------------------------------------------
//  Return pointer to outer message address, if any
//  Caller should not modify the provided data

char *zmsg_address (zmsg_t *self)
{
    assert (self);

    if (self->_part_count)
        return (self->_part_data [0]);
    else
        return (NULL);
}


//  --------------------------------------------------------------------------
//  Wraps message in new address envelope
//  If delim is not null, creates two-part envelope

void zmsg_wrap (zmsg_t *self, char *address, char *delim)
{
    assert (self);
    assert (address);

    //  Push optional delimiter and then address
    if (delim)
        zmsg_push (self, delim);
    zmsg_push (self, address);
}


//  --------------------------------------------------------------------------
//  Unwraps outer message envelope and returns address
//  Discards empty message part after address, if any
//  Caller should free returned string when finished with it

char *zmsg_unwrap (zmsg_t *self)
{
    assert (self);

    char *address = zmsg_pop (self);
    if (*zmsg_address (self) == 0)
        free (zmsg_pop (self));
    return (address);
}


//  --------------------------------------------------------------------------
//  Runs self test of class

int zmsg_test (int verbose)
{
    zmsg_t
        *zmsg;

    printf (" * zmsg: ");

    //  Prepare our context and sockets
    void *context = zmq_init (1);
    void *output = zmq_socket (context, ZMQ_PUSH);
    assert (zmq_bind (output, "ipc://zmsg_selftest.ipc") == 0);
    void *input = zmq_socket (context, ZMQ_PULL);
    assert (zmq_connect (input, "ipc://zmsg_selftest.ipc") == 0);

    //  Test send and receive of single-part message
    zmsg = zmsg_new ();
    assert (zmsg);
    zmsg_set_body (zmsg, "Hello");
    assert (strcmp (zmsg_body (zmsg), "Hello") == 0);
    zmsg_send (&zmsg, output);
    assert (zmsg == NULL);

    zmsg = zmsg_recv (input);
    assert (zmsg_parts (zmsg) == 1);
    assert (strcmp (zmsg_body (zmsg), "Hello") == 0);

    //  Test send and receive of multi-part message
    zmsg = zmsg_new ();
    zmsg_set_body (zmsg, "Hello");
    zmsg_wrap     (zmsg, "address1", "");
    zmsg_wrap     (zmsg, "address2", NULL);
    assert (zmsg_parts (zmsg) == 4);
    zmsg_send (&zmsg, output);

    zmsg = zmsg_recv (input);
    assert (zmsg_parts (zmsg) == 4);
    assert (strcmp (zmsg_address (zmsg), "address2") == 0);
    zmsg_set_body (zmsg, "World");
    zmsg_send (&zmsg, output);

    zmsg = zmsg_recv (input);
    assert (zmsg_parts (zmsg) == 4);
    assert (strcmp (zmsg_body (zmsg), "World") == 0);
    char *part;
    part = zmsg_unwrap (zmsg);
    assert (strcmp (part, "address2") == 0);
    free (part);

    //  Pull off address 1, check that empty part was dropped
    part = zmsg_unwrap (zmsg);
    assert (strcmp (part, "address1") == 0);
    assert (zmsg_parts (zmsg) == 1);
    free (part);

    //  Check that message body was correctly modified
    part = zmsg_pop (zmsg);
    assert (strcmp (part, "World") == 0);
    assert (zmsg_parts (zmsg) == 0);

    zmsg_destroy (&zmsg);
    assert (zmsg == NULL);

    printf ("OK\n");
//? broken    zmq_term (context);
    return 0;
}
