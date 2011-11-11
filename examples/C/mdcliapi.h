/*  =====================================================================
    mdcliapi.h

    Majordomo Protocol Client API
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

#ifndef __MDCLIAPI_H_INCLUDED__
#define __MDCLIAPI_H_INCLUDED__

#include "czmq.h"
#include "mdp.h"

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _mdcli_t mdcli_t;

mdcli_t *
    mdcli_new (char *broker, int verbose);
void
    mdcli_destroy (mdcli_t **self_p);
void
    mdcli_set_timeout (mdcli_t *self, int timeout);
void
    mdcli_set_retries (mdcli_t *self, int retries);
zmsg_t *
    mdcli_send (mdcli_t *self, char *service, zmsg_t **request_p);

#ifdef __cplusplus
}
#endif

#endif
