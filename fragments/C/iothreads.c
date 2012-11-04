int io-threads = 4;
void *context = zmq-ctx-new ();
zmq-ctx-set (context, ZMQ-IO-THREADS, io-threads);
assert (zmq-ctx-get (context, ZMQ-IO-THREADS) == io-threads);
