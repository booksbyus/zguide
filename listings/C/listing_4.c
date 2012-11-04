//  NOTE: do NOT reuse this example code!
static char *topic-str = "msg.x|";

void* pub-worker(void* arg){
    void *ctx = arg;
    assert(ctx);

    void *qskt = zmq-socket(ctx, ZMQ-REP);
    assert(qskt);

    int rc = zmq-connect(qskt, "inproc://querys");
    assert(rc == 0);

    void *pubskt = zmq-socket(ctx, ZMQ-PUB);
    assert(pubskt);

    rc = zmq-bind(pubskt, "inproc://publish");
    assert(rc == 0);

    uint8-t cmd;
    uint32-t nb;
    zmq-msg-t topic-msg, cmd-msg, nb-msg, resp-msg;

    zmq-msg-init-data(&topic-msg, topic-str, strlen(topic-str) , NULL, NULL);

    fprintf(stdout,"WORKER: ready to receive messages\n");
    //  NOTE: do NOT reuse this example code, It's broken.
    //  e.g. topic-msg will be invalid the second time through
    while (1){
    zmq-msg-send(pubskt, &topic-msg, ZMQ-SNDMORE);

    zmq-msg-init(&cmd-msg);
    zmq-msg-recv(qskt, &cmd-msg, 0);
    memcpy(&cmd, zmq-msg-data(&cmd-msg), sizeof(uint8-t));
    zmq-msg-send(pubskt, &cmd-msg, ZMQ-SNDMORE);
    zmq-msg-close(&cmd-msg);

    fprintf(stdout, "received cmd %u\n", cmd);

    zmq-msg-init(&nb-msg);
    zmq-msg-recv(qskt, &nb-msg, 0);
    memcpy(&nb, zmq-msg-data(&nb-msg), sizeof(uint32-t));
    zmq-msg-send(pubskt, &nb-msg, 0);
    zmq-msg-close(&nb-msg);

    fprintf(stdout, "received nb %u\n", nb);

    zmq-msg-init-size(&resp-msg, sizeof(uint8-t));
    memset(zmq-msg-data(&resp-msg), 0, sizeof(uint8-t));
    zmq-msg-send(qskt, &resp-msg, 0);
    zmq-msg-close(&resp-msg);

    }
    return NULL;
}
