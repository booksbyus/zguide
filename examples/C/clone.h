/*  =====================================================================
 *  clone - client-side Clone Pattern class
 *  ===================================================================== */

#ifndef __CLONE_INCLUDED__
#define __CLONE_INCLUDED__

#include "kvmsg.c"

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _clone_t clone_t;

clone_t *clone_new (void);
void clone_destroy (clone_t **self_p);
void clone_subtree (clone_t *self, char *subtree);
void clone_connect (clone_t *self, char *address, char *service);
void clone_set (clone_t *self, char *key, char *value, int ttl);
char *clone_get (clone_t *self, char *key);

#ifdef __cplusplus
}
#endif

#endif
