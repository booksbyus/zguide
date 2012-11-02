void
zre_peer_disconnect (zre_peer_t *self)
{
    //  If connected, destroy socket and drop all pending messages
    assert (self);
    if (self->connected) {
        zsocket_destroy (self->ctx, self->mailbox);
        free (self->endpoint);
        self->endpoint = NULL;
        self->connected = false;
    }
}
