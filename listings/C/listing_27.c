//  Create a new Binary Star instance, bind PUB and connect SUB
bstar_t *
    bstar_new (int primary, char *bind_to, char *connect_to);

//  Destroy a Binary Star instance
void
    bstar_destroy (bstar_t **self_p);

//  Listen on this endpoint for client votes
void
    bstar_listen (bstar_t *self, char *endpoint, int type);

//  Wait for valid activity on client socket, return socket
void *
    bstar_wait (bstar_t *self);
    
