zre_peer_t *peer = (zre_peer_t *) zhash_lookup (self->peers, identity);
if (peer == NULL) {
    peer = zre_peer_new (self->ctx, identity, self->peers);
    //  We can immediately start to send messages to peer
    zstr_sendm (self->pipe, "JOINED");
    zstr_send (self->pipe, identity);
}
//  Connect peer opportunistically
if (!zre_peer_connected (peer) && address)
    zre_peer_connect (peer, self->identity, address, port);

//  Any activity from the peer means it's alive
zre_peer_refresh (peer);
return peer;
