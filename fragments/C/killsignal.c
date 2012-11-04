void *control = zmq-socket (context, ZMQ-PUB);
zmq-bind (control, "tcp://*:5559");
...
//  Send kill signal to workers
zmq-msg-init-data (&message, "KILL", 5);
zmq-msg-send (control, &message, 0);
zmq-msg-close (&message);
