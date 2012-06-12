/*  =====================================================================
 *  mdwrkapi.h - Majordomo Protocol Worker API
 *  Implements the MDP/Worker spec at http://rfc.zeromq.org/spec:7.
 *  ===================================================================== */

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