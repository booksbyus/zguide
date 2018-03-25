/*
**Hello World server
REP socket bind to tcp://*:5555
expect receive"Hello"，use "World" to responsd
*/

#include "zhelpers.h"
#include <unistd.h>

int main (void)
{
    void *context = zmq_ctx_new ();
    void *responder = zmq_socket (context, ZMQ_REP);
    int rc = zmq_bind (responder, "tcp://*:5555");
    assert (rc == 0);
    while (1)
	{
        char buffer [10]={0};
        zmq_recv (responder, buffer, 10, 0);
        printf("Received request msg: %s\n", buffer);
        printf("Send respond msg: World\n\n");
        zmq_send (responder, "World", 5, 0);
        sleep (1);          //Do some work
    }
    return 0;
}
