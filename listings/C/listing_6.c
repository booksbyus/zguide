void *mousetrap;

//  Create socket for catching mice
mousetrap = zmq_socket (context, ZMQ_PULL);

//  Configure the socket
int64_t jawsize = 10000;
zmq_setsockopt (mousetrap, ZMQ_HWM, &jawsize, sizeof jawsize);

//  Plug socket into mouse hole
zmq_connect (mousetrap, "tcp://192.168.55.221:5001");

//  Wait for juicy mouse to arrive
zmq_msg_t mouse;
zmq_msg_init (&mouse);
zmq_recv (mousetrap, &mouse, 0);
//  Destroy the mouse
zmq_msg_close (&mouse);

//  Destroy the socket
zmq_close (mousetrap);
