static void *
worker-thread (void *arg) {
    void *context = arg;
    void *worker = zmq-socket (context, ZMQ-REP);
    assert (worker);
    int rc;
    rc = zmq-connect (worker, "ipc://worker");
    assert (rc == 0);

    void *broadcast = zmq-socket (context, ZMQ-PUB);
    assert (broadcast);
    rc = zmq-bind (broadcast, "ipc://publish");
    assert (rc == 0);

    while (1) {
        char *part1 = s-recv (worker);
        char *part2 = s-recv (worker);
        printf ("Worker got [%s][%s]\n", part1, part2);
        s-sendmore (broadcast, "msg");
        s-sendmore (broadcast, part1);
        s-send     (broadcast, part2);
        free (part1);
        free (part2);

        s-send (worker, "OK");
    }
    return NULL;
}
