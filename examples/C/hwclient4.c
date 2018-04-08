/*
Hello World client
DEALER socket connect to tcp://localhost:5555
send "Hello" to Server，expect recv "World" from Server
*/
#include "zhelpers.h"
#include <unistd.h>

int main (void)
{
    printf ("Connecting to hello world server...\n");
    void *context = zmq_ctx_new ();
    void *requester = zmq_socket (context, ZMQ_DEALER);
    //set socket identity 
    zmq_setsockopt(requester, ZMQ_IDENTITY, "ZMQ", strlen("ZMQ"));
    zmq_connect (requester, "tcp://localhost:5555");
    int request_nbr;
    for (request_nbr = 0; request_nbr < 10; request_nbr++)
	{
        printf ("Sending request msg: Hello NO=%d...\n", request_nbr+1);
		//send request msg
        s_sendmore(requester, "");   //send empty delimiter frame
        s_send(requester, "Hello");  //send data frame
		//recv reply msg
        char *empty=s_recv(requester);
        assert(empty[0]  == 0);
        char *reply=s_recv(requester);
        printf ("Received respond msg: %s NO=%d\n\n", reply,request_nbr+1);
        free(reply);
    }

    zmq_close(requester);
    zmq_ctx_destroy (context);
    return 0;
}
