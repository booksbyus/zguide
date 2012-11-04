//  Receive 0MQ string from socket and convert into C string
static char *
s-recv (void *socket) {
    zmq-msg-t message;
    zmq-msg-init (&message);
    int size = zmq-msg-recv (&message, socket, 0);
    if (size == -1)
        return NULL;
    char *string = malloc (size + 1);
    memcpy (string, zmq-msg-data (&message), size);
    zmq-msg-close (&message);
    string [size] = 0;
    return (string);
}

//  Convert C string to 0MQ string and send to socket
static int
s-send (void *socket, char *string) {
    zmq-msg-t message;
    zmq-msg-init-size (&message, strlen (string));
    memcpy (zmq-msg-data (&message), string, strlen (string));
    int size = zmq-msg-send (&message, socket, 0);
    zmq-msg-close (&message);
    return (size);
}
