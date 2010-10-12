/*
    Reproduces freeze in 0MQ/2.1
    1. Run, application hangs on zmq_close

    Works properly in 0MQ/2.0.9.
*/

#include <zmq.h>

int main (int argc, char *argv[])
{
    void *context = zmq_init (1);
    void *socket = zmq_socket (context, ZMQ_REQ);
    zmq_close (socket);
    zmq_term (context);
    return 0;
}
