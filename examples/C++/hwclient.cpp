//
//  Hello World client in C++
//  Connects REQ socket to tcp://localhost:5555
//  Sends "Hello" to server, expects "World" back
//
#include <zmq.hpp>
#include <string.h>
#include <stdio.h>

int main ()
{
    //  Prepare our context and socket
    zmq::context_t context (1);
    zmq::socket_t socket (context, ZMQ_REQ);

    printf ("Connecting to hello world server...\n");
    socket.connect ("tcp://localhost:5555");

    //  Do 10 requests, waiting each time for a response
    for (int request_nbr = 0; request_nbr != 10; request_nbr++) {
        zmq::message_t request (6);
        memcpy ((void *) request.data (), "Hello", 6);
        printf ("Sending request %d...\n", request_nbr);
        socket.send (request);

        //  Get the reply.
        zmq::message_t reply;
        socket.recv (&reply);
        printf ("Received reply %d: [%s]\n", request_nbr,
            (char *) reply.data ());
    }
    return 0;
}
