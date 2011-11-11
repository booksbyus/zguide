/*  =====================================================================
    flcliapi - Freelance Pattern agent class
    Model 3: uses ROUTER socket to address specific services

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

#ifndef __FLCLIAPI_INCLUDED__
#define __FLCLIAPI_INCLUDED__

#include "czmq.h"

//  We design our client API as a class

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _flcliapi_t flcliapi_t;

flcliapi_t *
    flcliapi_new (void);
void
    flcliapi_destroy (flcliapi_t **self_p);
void
    flcliapi_connect (flcliapi_t *self, char *endpoint);
zmsg_t *
    flcliapi_request (flcliapi_t *self, zmsg_t **request_p);

#ifdef __cplusplus
}
#endif

#endif
