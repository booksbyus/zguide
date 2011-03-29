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

#include "zhelpers.h"
#include "zhash.h"
#include "zmsg.h"

//  Opaque class structure
typedef struct _kvmsg kvmsg_t;

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
    kvmsg_store (kvmsg_t **self_p, zhash_t *hash);
void
    kvmsg_dump (kvmsg_t *self);
int
    kvmsg_test (int verbose);

#ifdef __cplusplus
}
#endif

#endif      //  Included
