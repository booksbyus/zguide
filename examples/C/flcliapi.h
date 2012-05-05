/*  =====================================================================
 *  flcliapi - Freelance Pattern agent class
 *  ===================================================================== */

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
