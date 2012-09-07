//  Send out heartbeats at regular intervals
uint64_t heartbeat_at = zclock_time () + HEARTBEAT_INTERVAL;

while (1) {
    ...
    int rc = zmq_poll (items, 1, HEARTBEAT_INTERVAL * ZMQ_POLL_MSEC);
    ...
    //  Send heartbeat to queue if it's time
    if (zclock_time () > heartbeat_at) {
        ... Send heartbeats to all peers that expect them
        //  Set timer for next heartbeat
        heartbeat_at = zclock_time () + HEARTBEAT_INTERVAL;
    }
}
