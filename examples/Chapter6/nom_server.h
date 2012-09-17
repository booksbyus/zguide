/*  =========================================================================
    nom_server.h

    Generated header for nom_server protocol server
    =========================================================================
*/

#ifndef __NOM_SERVER_H_INCLUDED__
#define __NOM_SERVER_H_INCLUDED__

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _nom_server_t nom_server_t;

//  Create a new nom_server
nom_server_t *
    nom_server_new (void);

//  Destroy the nom_server
void
    nom_server_destroy (nom_server_t **self_p);

//  Bind the nom_server to an endpoint
void
    nom_server_bind (nom_server_t *self, const char *endpoint);

//  Execute the nom_server until interrupted
void
    nom_server_wait (nom_server_t *self);

//  Self test of this class
int
    nom_server_test (bool verbose);

#ifdef __cplusplus
}
#endif

#endif
