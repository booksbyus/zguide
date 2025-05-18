//  Hello World server

#include <zmq.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>

int main (void)
{
    //  Socket to talk to clients
    void *context = zmq_ctx_new ();
    void *responder = zmq_socket (context, ZMQ_REP);
    int response = zmq_bind (responder, "tcp://*:5555");
    assert (response == 0); // Check if the response code indicates success

    while (1) {
        static const size_t kReadBufferLength = 10;
        char buffer [kReadBufferLength];
        zmq_recv (responder, buffer, kReadBufferLength, 0);
        printf ("Received Hello\n");
        sleep (1);          //  Pretend to do some 'work'
        static const char kReplyString[] = "World";
        zmq_send(responder, kReplyString, sizeof(kReplyString) - 1, 0);
    }
    return 0;
}
