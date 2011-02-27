//  Worker using REQ socket to do LRU routing
//
static void *
worker_thread (void *args) {
    void *context = zmq_init (1);
    void *worker = zmq_socket (context, ZMQ_REQ);
    s_set_id (worker);          //  Makes tracing easier
    zmq_connect (worker, "ipc://backend.ipc");

    //  Tell broker we're ready for work
    s_send (worker, "READY");

    while (1) {
        zmsg_t *zmsg = zmsg_recv (worker);
        printf ("Worker: %s\n", zmsg_body (zmsg));
        zmsg_body_set (zmsg, "OK");
        zmsg_send (&zmsg, worker);
    }
    zmq_close (worker);
    zmq_term (context);
    return NULL;
}

