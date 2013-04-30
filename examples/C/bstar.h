/*  =====================================================================
 *  bstar - Binary Star reactor
 *  ===================================================================== */

#ifndef __BSTAR_H_INCLUDED__
#define __BSTAR_H_INCLUDED__

#include "czmq.h"

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
bstar_t *bstar_new (int primary, char *local, char *remote);

//  Destroy a Binary Star instance
void bstar_destroy (bstar_t **self_p);

//  Return underlying zloop reactor, for timer and reader
//  registration and cancelation.
zloop_t *bstar_zloop (bstar_t *self);

//  Register voting reader
int bstar_voter (bstar_t *self, char *endpoint, int type,
                 zloop_fn handler, void *arg);

//  Register main state change handlers
void bstar_new_master (bstar_t *self, zloop_fn handler, void *arg);
void bstar_new_slave (bstar_t *self, zloop_fn handler, void *arg);

//  Enable/disable verbose tracing
void bstar_set_verbose (bstar_t *self, bool verbose);

//  Start the reactor, ends if a callback function returns -1, or the
//  process received SIGINT or SIGTERM.
int bstar_start (bstar_t *self);

#ifdef __cplusplus
}
#endif

#endif
