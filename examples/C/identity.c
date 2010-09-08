//
//  Demonstrate identities as used by the request-reply
//  pattern.  Run this program by itself.
//
#include "zhelpers.h"

//  Receives all message parts from socket, prints neatly
//
static void
s_dump_input (void *socket)
{
    puts ("----------------------------------------");
    while (1) {
        //  Process all parts of the message
        zmq_msg_t message;
        zmq_msg_init (&message);
        zmq_recv (socket, &message, 0);

        //  Dump the message as text or binary
        char *data = zmq_msg_data (&message);
        int size = zmq_msg_size (&message);
        int is_text = 1;
        int char_nbr;
        for (char_nbr = 0; char_nbr < size; char_nbr++)
            if (data [char_nbr] < 32 || data [char_nbr] > 127)
                is_text = 0;

        printf ("[%03d] ", size);
        for (char_nbr = 0; char_nbr < size; char_nbr++) {
            if (is_text)
                printf ("%c", data [char_nbr]);
            else
                printf ("%02X", (unsigned char) data [char_nbr]);
        }
        printf ("\n");

        int64_t more;           //  Multipart detection
        size_t more_size = sizeof (more);
        zmq_getsockopt (socket, ZMQ_RCVMORE, &more, &more_size);
        zmq_msg_close (&message);
        if (!more)
            break;      //  Last message part
    }
}


int main () {
    void *context = zmq_init (1);

    //  First allow 0MQ to set the identity
    void *sink = zmq_socket (context, ZMQ_XREP);
    zmq_bind (sink, "inproc://example");

    void *anonymous = zmq_socket (context, ZMQ_REQ);
    zmq_connect (anonymous, "inproc://example");
    s_send (anonymous, "XREP uses a generated UUID");
    s_dump_input (sink);

    void *identified = zmq_socket (context, ZMQ_REQ);
    zmq_setsockopt (identified, ZMQ_IDENTITY, "Hello", 5);
    zmq_connect (identified, "inproc://example");
    s_send (identified, "XREP socket uses REQ's socket identity");
    s_dump_input (sink);

    return 0;
}
