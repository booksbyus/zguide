/*
Hello World server
ROUTER socket bind to "tcp://*:5555"
expect receive "Hello", use "World" reply
*/
#include "zhelpers.h"
#include <unistd.h>

int main (void)
{
    void *context = zmq_ctx_new ();
    void *responder = zmq_socket (context, ZMQ_ROUTER);
    int rc = zmq_bind (responder, "tcp://*:5555");
    assert (rc == 0);

    while (1)
	{
        char *identity, *string, *content;
        //receive client request msg
        identity=s_recv(responder);  //receive the first part is identity frame
        printf("[Frame_1] identity = %s\n",identity);
        //if the second part is empty frame, then continue receiving the third part
        string = s_recv(responder);
        printf("[Frame_2] string = %s\n", string);
        if(strcmp(string,"") == 0){
            //receive the third part is the data frame
            content = s_recv(responder);
            printf("[Frame_3] Received request msg: %s\n", content);
            free(string);
            free(content);
        }
        //if the second part is not empty frame,then discard the whole ZMQ msg.
        else{
            printf("Discard the ZMQ message!\n");
            free(string);
            free(identity);
            continue;
        }
        sleep (1);   //Do some work
        //send reply msg to client
        printf("Send respond msg: World\n\n");
        s_sendmore(responder,identity);  //send identity frame
        s_sendmore(responder,"");  //send empty frame
        zmq_send (responder, "World", 5, 0);  //send data frame
        free(identity);
    }
    zmq_close(responder);
    zmq_ctx_destroy(context);
    return 0;
}
