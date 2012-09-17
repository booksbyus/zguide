/*  =========================================================================
    mdp_client.h

    Generated codec header for mdp_client
    =========================================================================
*/

#ifndef __MDP_CLIENT_H_INCLUDED__
#define __MDP_CLIENT_H_INCLUDED__

/*  These are the mdp_client messages

    REQUEST - Client request to broker
        service       string 
        body          frame 

    REPLY - Response back to client
        service       string 
        body          frame 
*/


#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _mdp_client_t mdp_client_t;

//  Create a new mdp_client
mdp_client_t *
    mdp_client_new (void);

//  Destroy the mdp_client
void
    mdp_client_destroy (mdp_client_t **self_p);

//  Receive and parse a mdp_client from the socket
mdp_client_t *
    mdp_client_recv (void *socket);

//  Send the mdp_client to the socket, and destroy it
int
    mdp_client_send (mdp_client_t **self_p, void *socket);

//  Get/set the message address
zframe_t *
    mdp_client_address (mdp_client_t *self);
void
    mdp_client_address_set (mdp_client_t *self, zframe_t *address);

//  Get/set the service field
char *
    mdp_client_service (mdp_client_t *self);
void
    mdp_client_service_set (mdp_client_t *self, char *format, ...);

//  Get/set the body field
zframe_t *
    mdp_client_body (mdp_client_t *self);
void
    mdp_client_body_set (mdp_client_t *self, zframe_t *frame);

//  Self test of this class
int
    mdp_client_test (bool verbose);

#ifdef __cplusplus
}
#endif

#endif
