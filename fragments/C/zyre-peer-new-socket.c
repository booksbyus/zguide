//  Create new outgoing socket (drop any messages in transit)
self->mailbox = zsocket_new (self->ctx, ZMQ_DEALER);

//  Set our caller "From" identity so that receiving node knows
//  who each message came from.
zsocket_set_identity (self->mailbox, reply_to);

//  Set a high-water mark that allows for reasonable activity
zsocket_set_sndhwm (self->mailbox, PEER_EXPIRED * 100);

//  Send messages immediately or return EAGAIN
zsocket_set_sndtimeo (self->mailbox, 0);

//  Connect through to peer node
zsocket_connect (self->mailbox, "tcp://%s", endpoint);
