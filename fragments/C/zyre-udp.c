//  Constructor
static udp_t *
    udp_new (int port_nbr);

//  Destructor
static void
    udp_destroy (udp_t **self_p);

//  Returns UDP socket handle
static int
    udp_handle (udp_t *self);

//  Send message using UDP broadcast
static void
    udp_send (udp_t *self, byte *buffer, size_t length);

//  Receive message from UDP broadcast
static ssize_t
    udp_recv (udp_t *self, byte *buffer, size_t length);
