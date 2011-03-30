    //  Send out heartbeats at regular intervals
    uint64_t heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;
    while (1) {
        ...
        zmq_poll (items, 1, HEARTBEAT_INTERVAL * 1000);
        ...
        //  Do this unconditionally, whatever zmq_poll did
        if (s_clock () > heartbeat_at) {
            ... Send heartbeats to all peers that expect them
            //  Set timer for next heartbeat
            heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;
        }
    }
