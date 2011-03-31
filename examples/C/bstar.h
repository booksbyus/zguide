/*  =========================================================================
    bstar - Binary Star server core

    -------------------------------------------------------------------------
    Copyright (c) 1991-2011 iMatix Corporation <www.imatix.com>
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

#ifndef __BSTAR_H_INCLUDED__
#define __BSTAR_H_INCLUDED__

#include "zmsg.h"

//  Arguments for constructor
#define BSTAR_PRIMARY   1
#define BSTAR_BACKUP    0

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _bstar_t bstar_t;

//  Create a new Binary Star instance, bind PUB and connect SUB
bstar_t *
    bstar_new (int primary, char *bind_to, char *connect_to);

//  Destroy a Binary Star instance
void
    bstar_destroy (bstar_t **self_p);

//  Listen on this endpoint for client votes
void
    bstar_listen (bstar_t *self, char *endpoint, int type);

//  Wait for valid activity on client socket, return socket
void *
    bstar_wait (bstar_t *self);

#ifdef __cplusplus
}
#endif

#endif
