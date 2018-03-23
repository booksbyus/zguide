/**
**Hello World client
DEALER socket connect to tcp://localhost:5555
send "Hello" to server，expect receive "World"
*/
#include "zhelpers.h"
#include <unistd.h>

int main (void)
{
	printf ("Connecting to hello world server...\n");
	void *context = zmq_ctx_new ();
	void *requester = zmq_socket (context, ZMQ_DEALER);
	zmq_connect (requester, "tcp://localhost:5555");
	int request_nbr;	//request number
	int reply_nbr = 0;  //receive respond number
	for (request_nbr = 0; request_nbr < 10; request_nbr++)
	{
		char buffer [10];
		memset(buffer,0,sizeof(buffer));
		printf ("Sending request msg: Hello NO=%d...\n", request_nbr+1);
		//send request msg to server
		s_sendmore(requester,"");  //send multi part msg,the first part is empty part
		zmq_send (requester, "Hello", 5, 0); //the second part is your request msg
		//receive reply msg
		int len;
		len = zmq_recv (requester, buffer, 10, 0);
		if(len == -1){
			printf("Error:%s\n", zmq_strerror(errno));
			exit(-1);
		}
		//if the first part you received is empty part,then continue receiving next part 
		if (strcmp(buffer,"") == 0){
			memset(buffer,0,sizeof(buffer));
			len = zmq_recv(requester, buffer, 10, 0);
			if(len == -1){
				printf("Error:%s\n", zmq_strerror(errno));
				exit(-1);
			}
			printf("Received respond msg: %s NO=%d\n\n", buffer,++reply_nbr);
		}
		//if the first part you received is not empty part,discard the whole ZMQ msg
		else{
			printf("Discard the ZMQ message!\n",buffer);
		}
	}
	zmq_close(requester);
	zmq_ctx_destroy (context);
	return 0;
}
