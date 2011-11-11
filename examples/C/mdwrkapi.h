/*  =====================================================================
    mdwrkapi.h

    Majordomo Protocol Worker API
    Implements the MDP/Worker spec at http://rfc.zeromq.org/spec:7.

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

#ifndef __MDWRKAPI_H_INCLUDED__
#define __MDWRKAPI_H_INCLUDED__

#include "czmq.h"
#include "mdp.h"

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _mdwrk_t mdwrk_t;

mdwrk_t *
    mdwrk_new (char *broker,char *service, int verbose);
void
    mdwrk_destroy (mdwrk_t **self_p);
void
    mdwrk_set_liveness (mdwrk_t *self, int liveness);
void
    mdwrk_set_heartbeat (mdwrk_t *self, int heartbeat);
void
    mdwrk_set_reconnect (mdwrk_t *self, int reconnect);
zmsg_t *
    mdwrk_recv (mdwrk_t *self, zmsg_t **reply_p);

#ifdef __cplusplus
}
#endif

#endif