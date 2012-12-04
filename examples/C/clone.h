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

//  Create a new clone class instance
clone_t *
    clone_new (void);

//  Destroy a clone class instance
void
    clone_destroy (clone_t **self_p);

//  Define the subtree, if any, for this clone class
void
    clone_subtree (clone_t *self, char *subtree);

//  Connect the clone class to one server
void
    clone_connect (clone_t *self, char *address, char *service);

//  Set a value in the shared hashmap
void
    clone_set (clone_t *self, char *key, char *value, int ttl);
    
//  Get a value from the shared hashmap
char *
    clone_get (clone_t *self, char *key);

#ifdef __cplusplus
}
#endif

#endif
