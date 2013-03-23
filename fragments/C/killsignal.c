void *control = zmq_socket (context, ZMQ_PUB);
zmq_bind (control, "tcp://*:5559");
...
//  Send kill signal to workers
s_send (controller, "KILL");
