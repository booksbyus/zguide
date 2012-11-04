void my-free (void *data, void *hint) {
    free (data);
}
//  Send message from buffer, which we allocate and 0MQ will free for us
zmq-msg-t message;
zmq-msg-init-data (&message, buffer, 1000, my-free, NULL);
zmq-msg-send (socket, &message, 0);
