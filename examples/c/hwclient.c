//
//  Hello World client in C
//  Connects REQ socket to tcp://localhost:5555
//  Sends "Hello" to server, expects "World" back
//
#include <zmq.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

int main () {
    void *context;          //  ØMQ context for our process
    void *socket;           //  Socket to talk to server
    int request_nbr;

    //  Prepare our context and socket
    context = zmq_init (1);
    socket = zmq_socket (context, ZMQ_REQ);

    printf ("Connecting to hello world server...\n");
    zmq_connect (socket, "tcp://localhost:5555");
    for (request_nbr = 0; request_nbr != 10; request_nbr++) {
        zmq_msg_t request, reply;

        zmq_msg_init_data (&request, "Hello", 6, NULL, NULL);
        printf ("Sending request %d...\n", request_nbr);
        zmq_send (socket, &request, 0);
        zmq_msg_close (&request);

        zmq_msg_init (&reply);
        zmq_recv (socket, &reply, 0);
        printf ("Received reply %d: [%s]\n", request_nbr,
            (char *) zmq_msg_data (&reply));
        zmq_msg_close (&reply);
    }
    return 0;
}
