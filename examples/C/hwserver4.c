/*
Hello World server
ROUTER socket bind to "tcp://*:5555"
+expect receive "Hello", use "World" reply
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
        char identity[10]={0};
        //recv client`s request msg
        //the 1st received frame is identity frame
        char *frame1=s_recv(responder);
        printf("[recv] frame1 = %s\n",frame1);
        memcpy(identity, frame1, strlen(frame1)+1);  //save the identity to char array identity
        free(frame1);
        //the 2nd frame received is empty delimiter frame
        char *frame2= s_recv(responder);
        printf("[recv] frame2 = %s\n", frame2);
        assert(frame2[0] == 0);
        free(frame2);
        //the 3rd frame received is data frame
        char *frame3=s_recv(responder);
        printf("[recv] frame3 = %s\n", frame3);
        free(frame3);

        sleep (1);   //Do some work

        //send respond msg to client
        printf("[send] frame1 = %s\n", identity);
        printf("[send] frame2 = %s\n", "");
        printf("[send] frame3 = %s\n\n", "World");
        s_sendmore(responder,identity);  //send identity frame
        s_sendmore(responder,"");        //send empty delimiter frame
        s_send (responder, "World");     //send data frame
        memset(identity, 0, sizeof(identity));  //clear up identity array
    }

    zmq_close(responder);
    zmq_ctx_destroy(context);
    return 0;
}
