/*  =====================================================================
 *  mdcliapi2.h - Majordomo Protocol Client API
 *  Implements the MDP/Worker spec at http://rfc.zeromq.org/spec:7.
 *  ===================================================================== */

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
int
    mdcli_send (mdcli_t *self, char *service, zmsg_t **request_p);
zmsg_t *
    mdcli_recv (mdcli_t *self);

#ifdef __cplusplus
}
#endif

#endif
