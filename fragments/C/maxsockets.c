int max_sockets = 1024;
void *context = zmq_ctx_new ();
zmq_ctx_get (context, ZMQ_MAX_SOCKETS, max_sockets);
assert (zmq_ctx_get (context, ZMQ_MAX_SOCKETS) == max_sockets);
