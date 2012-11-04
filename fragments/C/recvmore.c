while (1) {
    zmq-msg-t message;
    zmq-msg-init (&message);
    zmq-msg-recv (socket, &message, 0);
    //  Process the message frame
    zmq-msg-close (&message);
    int-t more;
    size-t more-size = sizeof (more);
    zmq-getsockopt (socket, ZMQ-RCVMORE, &more, &more-size);
    if (!more)
        break;      //  Last message frame
}
