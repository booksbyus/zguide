/*  =========================================================================
    mdp_worker.h

    Generated codec header for mdp_worker
    =========================================================================
*/

#ifndef __MDP_WORKER_H_INCLUDED__
#define __MDP_WORKER_H_INCLUDED__

/*  These are the mdp_worker messages

    READY - Worker tells broker it is ready
        service       string 

    REQUEST - Client request to broker
        client        frame 
        body          frame 

    REPLY - Worker returns reply to broker
        client        frame 
        body          frame 

    HEARBEAT - Either peer tells the other it's still alive

    DISCONNECT - Either peer tells other the party is over
*/

#define MDP_WORKER_READY                1
#define MDP_WORKER_REQUEST              2
#define MDP_WORKER_REPLY                3
#define MDP_WORKER_HEARBEAT             4
#define MDP_WORKER_DISCONNECT           5

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _mdp_worker_t mdp_worker_t;

//  Create a new mdp_worker
mdp_worker_t *
    mdp_worker_new (int id);

//  Destroy the mdp_worker
void
    mdp_worker_destroy (mdp_worker_t **self_p);

//  Receive and parse a mdp_worker from the socket
mdp_worker_t *
    mdp_worker_recv (void *socket);

//  Send the mdp_worker to the socket, and destroy it
int
    mdp_worker_send (mdp_worker_t **self_p, void *socket);

//  Get/set the message address
zframe_t *
    mdp_worker_address (mdp_worker_t *self);
void
    mdp_worker_address_set (mdp_worker_t *self, zframe_t *address);

//  Get/set the mdp_worker id
int
    mdp_worker_id (mdp_worker_t *self);
void
    mdp_worker_id_set (mdp_worker_t *self, int id);
    
//  Get/set the service field
char *
    mdp_worker_service (mdp_worker_t *self);
void
    mdp_worker_service_set (mdp_worker_t *self, char *format, ...);

//  Get/set the client field
zframe_t *
    mdp_worker_client (mdp_worker_t *self);
void
    mdp_worker_client_set (mdp_worker_t *self, zframe_t *frame);

//  Get/set the body field
zframe_t *
    mdp_worker_body (mdp_worker_t *self);
void
    mdp_worker_body_set (mdp_worker_t *self, zframe_t *frame);

//  Self test of this class
int
    mdp_worker_test (bool verbose);

#ifdef __cplusplus
}
#endif

#endif
