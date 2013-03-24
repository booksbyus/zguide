private static final int HEARTBEAT_LIVENESS = 3;       //  3-5 is reasonable
private static final int HEARTBEAT_INTERVAL = 1000;    //  msecs
private static final int INTERVAL_INIT = 1000;    //  Initial reconnect
private static final int INTERVAL_MAX = 32000;    //  After exponential backoff

...
//  If liveness hits zero, queue is considered disconnected
int liveness = HEARTBEAT_LIVENESS;
int interval = INTERVAL_INIT;

//  Send out heartbeats at regular intervals
long heartbeat_at = System.currentTimeMillis() + HEARTBEAT_INTERVAL;

while (true) {
    PollItem items[] = { new PollItem(worker, ZMQ.Poller.POLLIN) };
    int rc = ZMQ.poll(items, 1, HEARTBEAT_INTERVAL);

    if (items[0].isReadable()) {
        //  Receive any message from queue
        liveness = HEARTBEAT_LIVENESS;
        interval = INTERVAL_INIT;
    }
    else
    if (--liveness == 0) {
        Thread.sleep(interval);
        if (interval < INTERVAL_MAX)
            interval *= 2;
        ctx.destroySocket(worker);
        ...
        liveness = HEARTBEAT_LIVENESS;
    }
    //  Send heartbeat to queue if it's time
    if (System.currentTimeMillis() > heartbeat_at) {
        heartbeat_at = System.currentTimeMillis() + HEARTBEAT_INTERVAL;
        //  Send heartbeat message to queue
    }
}
