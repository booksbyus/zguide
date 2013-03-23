//  Get beacon frame from network
beacon_t beacon;
ssize_t size = zre_udp_recv (self->udp,
    (byte *) &beacon, sizeof (beacon_t));

//  Basic validation on the frame
if (size != sizeof (beacon_t)
||  beacon.protocol [0] != 'Z'
||  beacon.protocol [1] != 'R'
||  beacon.protocol [2] != 'E'
||  beacon.version != BEACON_VERSION)
    return 0;               //  Ignore invalid beacons

//  If we got a UUID and it's not our own beacon, we have a peer
if (memcmp (beacon.uuid, self->uuid, sizeof (uuid_t))) {
    char *identity = s_uuid_str (beacon.uuid);
    s_require_peer (self, identity,
        zre_udp_from (self->udp), ntohs (beacon.port));
    free (identity);
}
