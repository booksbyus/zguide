/*
Hello World client
REQ socket connect to tcp://localhost:5555
send "Hello" to Server，expect receive "World" from Server
*/
#include "zhelpers.h"
#include <unistd.h>

int main (void)
{
    printf ("Connecting to hello world server...\n");
    void *context = zmq_ctx_new ();
    void *requester = zmq_socket (context, ZMQ_REQ);
    //set REQ socket identity
    zmq_setsockopt(requester, ZMQ_IDENTITY, "ZMQ", strlen("ZMQ"));
    zmq_connect (requester, "tcp://localhost:5555");
    int request_nbr;
    for (request_nbr = 0; request_nbr < 10; request_nbr++)
	{
        char buffer [10]={0};
		//send request msg
        printf ("Sending request msg: Hello NO=%d...\n", request_nbr+1);
        zmq_send (requester, "Hello", 5, 0);
		//receive respond msg
        zmq_recv (requester, buffer, 10, 0);
        printf ("Received respond msg: %s NO=%d\n\n", buffer,request_nbr+1);
    }
    zmq_close(requester);
    zmq_ctx_destroy (context);
    return 0;
}
