/*  =====================================================================
    bstar - Binary Star reactor

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

#ifndef __BSTAR_H_INCLUDED__
#define __BSTAR_H_INCLUDED__

#include "zapi.h"

//  Arguments for constructor
#define BSTAR_PRIMARY   1
#define BSTAR_BACKUP    0

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _bstar_t bstar_t;

//  Create a new Binary Star instance, using local (bind) and
//  remote (connect) endpoints to set-up the server peering.
bstar_t *
    bstar_new (int primary, char *local, char *remote);

//  Destroy a Binary Star instance
void
    bstar_destroy (bstar_t **self_p);

//  Return underlying zloop reactor, for timer and reader
//  registration and cancelation.
zloop_t *
    bstar_zloop (bstar_t *self);

//  Register voting reader
int
    bstar_voter (bstar_t *self, char *endpoint, int type,
                 zloop_fn handler, void *arg);

//  Register failover handler
void
    bstar_failover (bstar_t *self, zloop_fn handler, void *arg);

//  Start the reactor, ends if a callback function returns -1, or the
//  process received SIGINT or SIGTERM.
int
    bstar_start (bstar_t *self);

//  Returns TRUE if the current server is master in the pair,
//  false if it is slave.
Bool
    bstar_master (bstar_t *self);

#ifdef __cplusplus
}
#endif

#endif
