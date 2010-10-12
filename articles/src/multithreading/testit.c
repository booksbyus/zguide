//
//  Show inter-thread signalling using 0MQ sockets
//
#include "zmsg.c"

int main () {
    void *context = zmq_init (1);
    void *socket = zmq_socket (context, ZMQ_PUB);
    s_version ();

    assert (zmq_bind (socket, "ipc://signal") == 0);
    zmsg_t *zmsg = zmsg_new ();
    zmsg_body_set (zmsg, "happy");
    zmsg_send (&zmsg, socket);

    sleep (1);
    zmq_term (context);
    return 0;
}
