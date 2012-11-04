void *context = zmq-ctx-new ();
assert (context);
void *socket = zmq-socket (context, ZMQ-REP);
assert (socket);
int rc = zmq-bind (socket, "tcp://*:5555");
if (rc != 0) {
    printf ("E: bind failed: %s\n", strerror (errno));
    return -1;
}
