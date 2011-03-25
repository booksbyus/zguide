/*  =====================================================================
    kvmsg.h

    Key-value message class for example applications

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

#ifndef __KVMSG_H_INCLUDED__
#define __KVMSG_H_INCLUDED__

#include "zhelpers.h"
#include "zhash.h"

//  Opaque class structure
typedef struct _kvmsg kvmsg_t;
typedef unsigned char byte;

#ifdef __cplusplus
extern "C" {
#endif

kvmsg_t *
    kvmsg_new (int64_t sequence);
void
    kvmsg_destroy (kvmsg_t **self_p);
int64_t
    kvmsg_sequence (kvmsg_t *self);
char *
    kvmsg_key (kvmsg_t *self);
byte *
    kvmsg_body (kvmsg_t *self);
size_t
    kvmsg_size (kvmsg_t *self);
void
    kvmsg_set_sequence (kvmsg_t *self, int64_t sequence);
void
    kvmsg_set_key (kvmsg_t *self, char *key);
void
    kvmsg_set_body (kvmsg_t *self, byte *body, size_t size);
void
    kvmsg_fmt_key (kvmsg_t *self, char *format, ...);
void
    kvmsg_fmt_body (kvmsg_t *self, char *format, ...);
kvmsg_t *
    kvmsg_recv (void *socket);
void
    kvmsg_send (kvmsg_t *self, void *socket);
void
    kvmsg_store (kvmsg_t *self, zhash_t *hash);
void
    kvmsg_dump (kvmsg_t *self);
int
    kvmsg_test (int verbose);

#ifdef __cplusplus
}
#endif


//  Keys are short strings
#define KVMSG_KEY_MAX   255
#define KVMSG_FRAMES    3

//  Message is formatted on wire as three frames:
//  frame 0: key (0MQ string)
//  frame 1: sequence (8 bytes, network order)
//  frame 2: body (blob)

struct _kvmsg {
    //  Presence indicators for each frame
    int present [KVMSG_FRAMES];
    //  Corresponding 0MQ message frames, if any
    zmq_msg_t frame [KVMSG_FRAMES];
    //  Key, copied into safe C string
    char key [KVMSG_KEY_MAX + 1];
};


//  --------------------------------------------------------------------------
//  Constructor, sets initial body if provided

kvmsg_t *
kvmsg_new (int64_t sequence)
{
    kvmsg_t
        *self;

    self = (kvmsg_t *) calloc (1, sizeof (kvmsg_t));

    //  Create empty message frames
    int frame_nbr;
    for (frame_nbr = 0; frame_nbr < KVMSG_FRAMES; frame_nbr++)
        self->present [frame_nbr] = 0;
    
    *self->key = 0;
    kvmsg_set_sequence (self, sequence);
    return self;
}


//  --------------------------------------------------------------------------
//  Destructor

//  Free shim, compatible with zhash_free_fn
void
kvmsg_free (void *ptr)
{
    if (ptr) {
        kvmsg_t *self = (kvmsg_t *) ptr;
        //  Destroy message frames if any
        int frame_nbr;
        for (frame_nbr = 0; frame_nbr < KVMSG_FRAMES; frame_nbr++)
            if (self->present [frame_nbr])
                zmq_msg_close (&self->frame [frame_nbr]);

        //  Free object structure
        free (self);
    }
}

void
kvmsg_destroy (kvmsg_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        kvmsg_free (*self_p);
        *self_p = NULL;
    }
}


//  --------------------------------------------------------------------------
//  Return sequence nbr from last read message, if any

int64_t
kvmsg_sequence (kvmsg_t *self)
{
    assert (self);
    if (self->present [0]) {
        assert (zmq_msg_size (&self->frame [0]) == 8);
        byte *source = zmq_msg_data (&self->frame [0]);
        int64_t sequence = ((int64_t) (source [0]) << 56)
                         + ((int64_t) (source [1]) << 48)
                         + ((int64_t) (source [2]) << 40)
                         + ((int64_t) (source [3]) << 32)
                         + ((int64_t) (source [4]) << 24)
                         + ((int64_t) (source [5]) << 16)
                         + ((int64_t) (source [6]) << 8)
                         +  (int64_t) (source [7]);
        return sequence;
    }
    else
        return 0;
}


//  --------------------------------------------------------------------------
//  Return key from last read message, if any, else NULL

char *
kvmsg_key (kvmsg_t *self)
{
    assert (self);
    if (self->present [1]) {
        if (!*self->key) {
            size_t size = zmq_msg_size (&self->frame [1]);
            if (size > KVMSG_KEY_MAX)
                size = KVMSG_KEY_MAX;
            memcpy (self->key, zmq_msg_data (&self->frame [1]), size);
            self->key [size] = 0;
        }
        return self->key;
    }
    else
        return NULL;
}


//  --------------------------------------------------------------------------
//  Return body from last read message, if any, else NULL

byte *
kvmsg_body (kvmsg_t *self)
{
    assert (self);
    if (self->present [2])
        return (byte *) zmq_msg_data (&self->frame [2]);
    else
        return NULL;
}


//  --------------------------------------------------------------------------
//  Return body size from last read message, if any, else zero

size_t
kvmsg_size (kvmsg_t *self)
{
    assert (self);
    if (self->present [2])
        return zmq_msg_size (&self->frame [2]);
    else
        return 0;
}


//  --------------------------------------------------------------------------
//  Set message sequence number

void
kvmsg_set_sequence (kvmsg_t *self, int64_t sequence)
{
    assert (self);
    if (self->present [0])
        zmq_msg_close (&self->frame [0]);
    self->present [0] = 1;
    zmq_msg_init_size (&self->frame [0], 8);

    byte *source = zmq_msg_data (&self->frame [0]);
    source [0] = (byte) ((sequence >> 56) & 255);
    source [1] = (byte) ((sequence >> 48) & 255);
    source [2] = (byte) ((sequence >> 40) & 255);
    source [3] = (byte) ((sequence >> 32) & 255);
    source [4] = (byte) ((sequence >> 24) & 255);
    source [5] = (byte) ((sequence >> 16) & 255);
    source [6] = (byte) ((sequence >> 8)  & 255);
    source [7] = (byte) ((sequence)       & 255);
}


//  --------------------------------------------------------------------------
//  Set message key

void
kvmsg_set_key (kvmsg_t *self, char *key)
{
    assert (self);
    if (self->present [1])
        zmq_msg_close (&self->frame [1]);
    self->present [1] = 1;
    zmq_msg_init_size (&self->frame [1], strlen (key));
    memcpy (zmq_msg_data (&self->frame [1]), key, strlen (key));
}


//  --------------------------------------------------------------------------
//  Set message body

void
kvmsg_set_body (kvmsg_t *self, byte *body, size_t size)
{
    assert (self);
    if (self->present [2])
        zmq_msg_close (&self->frame [2]);
    self->present [2] = 1;
    zmq_msg_init_size (&self->frame [2], size);
    memcpy (zmq_msg_data (&self->frame [2]), body, size);
}


//  --------------------------------------------------------------------------
//  Set message key using printf format

void
kvmsg_fmt_key (kvmsg_t *self, char *format, ...)
{
    char value [KVMSG_KEY_MAX + 1];
    va_list args;

    assert (self);
    va_start (args, format);
    vsnprintf (value, KVMSG_KEY_MAX, format, args);
    va_end (args);
    kvmsg_set_key (self, value);
}


//  --------------------------------------------------------------------------
//  Set message body using printf format

void
kvmsg_fmt_body (kvmsg_t *self, char *format, ...)
{
    char value [255 + 1];
    va_list args;

    assert (self);
    va_start (args, format);
    vsnprintf (value, 255, format, args);
    va_end (args);
    kvmsg_set_body (self, (byte *) value, strlen (value));
}


//  --------------------------------------------------------------------------
//  Reads KV message from socket, returns new kvmsg instance.

kvmsg_t *
kvmsg_recv (void *socket)
{
    assert (socket);
    kvmsg_t *self = kvmsg_new (0);

    //  Read all frames off the wire, reject if bogus
    int frame_nbr;
    for (frame_nbr = 0; frame_nbr < KVMSG_FRAMES; frame_nbr++) {
        if (self->present [frame_nbr])
            zmq_msg_close (&self->frame [frame_nbr]);
        zmq_msg_init (&self->frame [frame_nbr]);
        self->present [frame_nbr] = 1;
        if (zmq_recv (socket, &self->frame [frame_nbr], 0)) {
            kvmsg_destroy (&self);
            break;
        }
        //  Verify multipart framing
        int64_t more;
        size_t more_size = sizeof (more);
        zmq_getsockopt (socket, ZMQ_RCVMORE, &more, &more_size);
        int expected = (frame_nbr < KVMSG_FRAMES - 1)? 1: 0;
        if (more != expected) {
            kvmsg_destroy (&self);
            break;
        }
    }
    return self;
}


//  --------------------------------------------------------------------------
//  Send key-value message to socket

void
kvmsg_send (kvmsg_t *self, void *socket)
{
    assert (self);
    assert (socket);

    int frame_nbr;
    for (frame_nbr = 0; frame_nbr < KVMSG_FRAMES; frame_nbr++) {
        assert (self->present [frame_nbr]);
        zmq_msg_t copy;
        zmq_msg_init (&copy);
        zmq_msg_copy (&copy, &self->frame [frame_nbr]);
        zmq_send (socket, &copy,
            (frame_nbr < KVMSG_FRAMES - 1)? ZMQ_SNDMORE: 0);
        zmq_msg_close (&copy);
    }
}


//  --------------------------------------------------------------------------
//  Store entire kvmsg into hash map, if key/value are set
//  If you do this, don't destroy the kvmsg, it'll be destroyed automatically
//  when needed.

void
kvmsg_store (kvmsg_t *self, zhash_t *hash)
{
    assert (self);
    if (self->present [1]
    &&  self->present [2]) {
        zhash_update (hash, kvmsg_key (self), self);
        zhash_freefn (hash, kvmsg_key (self), kvmsg_free);
    }
}


//  --------------------------------------------------------------------------
//  Dump message to stderr, for debugging and tracing

void
kvmsg_dump (kvmsg_t *self)
{
    if (self) {
        fprintf (stderr, "--------------------------------------\n");
        if (!self) {
            fprintf (stderr, "NULL");
            return;
        }
        size_t size = kvmsg_size (self);
        byte  *body = kvmsg_body (self);
        fprintf (stderr, "[%" PRId64 "]\n", kvmsg_sequence (self));
        fprintf (stderr, "[%s]\n", kvmsg_key (self));
        fprintf (stderr, "[%03zd] ", size);

        int char_nbr;
        for (char_nbr = 0; char_nbr < size; char_nbr++)
            fprintf (stderr, "%02X", body [char_nbr]);
        fprintf (stderr, "\n");
    }
    else
        fprintf (stderr, "NULL message\n");
}


//  --------------------------------------------------------------------------
//  Runs self test of class

int
kvmsg_test (int verbose)
{
    kvmsg_t
        *kvmsg;

    printf (" * kvmsg: ");

    //  Prepare our context and sockets
    void *context = zmq_init (1);
    void *output = zmq_socket (context, ZMQ_XREQ);
    int rc = zmq_bind (output, "ipc://kvmsg_selftest.ipc");
    assert (rc == 0);
    void *input = zmq_socket (context, ZMQ_XREQ);
    rc = zmq_connect (input, "ipc://kvmsg_selftest.ipc");
    assert (rc == 0);

    zhash_t *kvmap = zhash_new ();
    
    //  Test send and receive of single-part message
    kvmsg = kvmsg_new (1);
    kvmsg_set_key (kvmsg, "key");
    kvmsg_set_body (kvmsg, (byte *) "body", 4);
    if (verbose)
        kvmsg_dump (kvmsg);
    kvmsg_send (kvmsg, output);
    kvmsg_store (kvmsg, kvmap);

    kvmsg = kvmsg_recv (input);
    if (verbose)
        kvmsg_dump (kvmsg);
    assert (strcmp (kvmsg_key (kvmsg), "key") == 0);
    kvmsg_store (kvmsg, kvmap);

    //  Should destroy all messages stored in map
    zhash_destroy (&kvmap);

    zmq_close (input);
    zmq_close (output);
    zmq_term (context);
    
    printf ("OK\n");
    return 0;
}

#endif      //  Included
