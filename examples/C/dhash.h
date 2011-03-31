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

#ifndef __DHASH_INCLUDED__
#define __DHASH_INCLUDED__

#include "kvmsg.c"

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _dhash_t dhash_t;

dhash_t *
    dhash_new (void);
void
    dhash_destroy (dhash_t **self_p);
void
    dhash_connect (dhash_t *self, char *address, int port);
void
    dhash_set (dhash_t *self, char *key, char *value);
char *
    dhash_get (dhash_t *self, char *key);

#ifdef __cplusplus
}
#endif

#endif
