//
//  Hello World server in C
//  Binds REP socket to tcp://*:5555
//  Expects "Hello" from client, replies with "World"
//

#include <zmq.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

int main () {
    void *context;          //  ØMQ context for our process
    void *socket;           //  Socket to talk to clients

    //  Prepare our context and socket
    context = zmq_init (1);
    socket = zmq_socket (context, ZMQ_REP);
    zmq_bind (socket, "tcp://*:5555");

    while (1) {
        zmq_msg_t request, reply;

        //  Wait for next request from client
        zmq_msg_init (&request);
        zmq_recv (socket, &request, 0);
        printf ("Received request: [%s]\n",
            (char *) zmq_msg_data (&request));
        zmq_msg_close (&request);

        //  Do some 'work'
        sleep (1);

        //  Send reply back to client
        zmq_msg_init_size (&reply, 6);
        memcpy ((void *) zmq_msg_data (&reply), "World", 6);
        zmq_send (socket, &reply, 0);
        zmq_msg_close (&reply);
    }
    return 0;
}
