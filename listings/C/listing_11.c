    void *control = zmq_socket (context, ZMQ_PUB);
    zmq_bind (control, "tcp://*:5559");
    ...
    //  Send kill signal to workers
    zmq_msg_init_data (&message, "KILL", 5);
    zmq_msg_send (control, &message, 0);
    zmq_msg_close (&message);
