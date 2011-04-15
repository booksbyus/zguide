/*  =====================================================================
    kvmsg - key-value message class for example applications

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

#include "zapi.h"

//  Opaque class structure
typedef struct _kvmsg kvmsg_t;

#ifdef __cplusplus
extern "C" {
#endif

//  Constructor, sets sequence as provided
kvmsg_t *
    kvmsg_new (int64_t sequence);
//  Destructor
void
    kvmsg_destroy (kvmsg_t **self_p);
//  Create duplicate of kvmsg
kvmsg_t *
    kvmsg_dup (kvmsg_t *self);

//  Return key from last read message, if any, else NULL
char *
    kvmsg_key (kvmsg_t *self);
//  Return sequence nbr from last read message, if any
int64_t
    kvmsg_sequence (kvmsg_t *self);
//  Return UUID from last read message, if any, else NULL
byte *
    kvmsg_uuid (kvmsg_t *self);
//  Return body from last read message, if any, else NULL
byte *
    kvmsg_body (kvmsg_t *self);
//  Return body size from last read message, if any, else zero
size_t
    kvmsg_size (kvmsg_t *self);

//  Set message key as provided
void
    kvmsg_set_key (kvmsg_t *self, char *key);
//  Set message sequence number
void
    kvmsg_set_sequence (kvmsg_t *self, int64_t sequence);
//  Set message UUID to generated value
void
    kvmsg_set_uuid (kvmsg_t *self);
//  Set message body
void
    kvmsg_set_body (kvmsg_t *self, byte *body, size_t size);
//  Set message key using printf format
void
    kvmsg_fmt_key (kvmsg_t *self, char *format, ...);
//  Set message body using printf format
void
    kvmsg_fmt_body (kvmsg_t *self, char *format, ...);

//  Reads key-value message from socket, returns new kvmsg instance.
kvmsg_t *
    kvmsg_recv (void *socket);
//  Send key-value message to socket; any empty frames are sent as such.
void
    kvmsg_send (kvmsg_t *self, void *socket);

//  Store entire kvmsg into hash map, if key/value are set
//  Nullifies kvmsg reference, and destroys automatically when no longer
//  needed.
void
    kvmsg_store (kvmsg_t **self_p, zhash_t *hash);
//  Dump message to stderr, for debugging and tracing
void
    kvmsg_dump (kvmsg_t *self);

//  Runs self test of class
int
    kvmsg_test (int verbose);

#ifdef __cplusplus
}
#endif

#endif      //  Included
