/*
    Reproduces segmentation fault in 0MQ/2.1
    1. Start in two windows
        myname d1 d2
        myname d2 d1
    2. Kill either instance
    3. Other instance crashes

#0  0x04f6e824 in ?? ()
#1  0x00152ac9 in zmq::session_t::activated (this=0x8054088, pipe_=0x8056598) at session.cpp:152
#2  0x0014c98c in zmq::reader_t::process_activate_reader (this=0x8056598) at pipe.cpp:140
#3  0x0014ac70 in zmq::object_t::process_command (this=0x8056598, cmd_=...) at object.cpp:61
#4  0x00147c2f in zmq::io_thread_t::in_event (this=0x804c8d0) at io_thread.cpp:82
#5  0x00146479 in zmq::epoll_t::loop (this=0x804c8f8) at epoll.cpp:197
#6  0x0014657d in zmq::epoll_t::worker_routine (arg_=0x804c8f8) at epoll.cpp:210
#7  0x0015a557 in zmq::thread_t::thread_routine (arg_=0x804c918) at thread.cpp:79
#8  0x001ad96e in start_thread () from /lib/tls/i686/cmov/libpthread.so.0
#9  0x002b4a4e in clone () from /lib/tls/i686/cmov/libc.so.6

    Trigger of crash is ZMQ_IDENTITY on sub socket; without this the code
    runs without crashing.

    Works properly in 0MQ/2.0.9.
*/
#include "zhelpers.h"

int main (int argc, char *argv[])
{
    //  Router master endpoint is first argument, remaining arguments
    //  are other master routers to slave to.
    //
    if (argc < 3) {
        printf ("syntax: dacenter this other ...\n");
        printf ("e.g. dacenter name1 name2...\n");
        exit (EXIT_FAILURE);
    }
    char *self = argv [1];
    printf ("I: preparing broker at %s...\n", self);

    //  Prepare our context and sockets
    void *context = zmq_init (1);
    char endpoint [256];

    //  Bind tweeter socket which we use to broadcast our
    //  The meta endpoint is for worker availability
    void *tweeter = zmq_socket (context, ZMQ_PUB);
    uint64_t hwm = 1;
    zmq_setsockopt (tweeter, ZMQ_HWM, &hwm, sizeof (hwm));
    snprintf (endpoint, 255, "ipc://%s-meta.ipc", self);
    assert (zmq_bind (tweeter, endpoint) == 0);

    //  Peer to each other brokers, by connecting slave and sensor
    int argn;
    void *sensor = zmq_socket (context, ZMQ_SUB);
    zmq_setsockopt (sensor, ZMQ_SUBSCRIBE, "STATUS", 6);
    //
    //  ***********************************************************
    //                     THIS CAUSES THE CRASH
    zmq_setsockopt (sensor, ZMQ_IDENTITY, self, strlen (self));
    //  ***********************************************************
    for (argn = 2; argn < argc; argn++) {
        char *peer = argv [argn];
        printf ("I: peering with broker at '%s'\n", peer);
        snprintf (endpoint, 255, "ipc://%s-meta.ipc", peer);
        assert (zmq_connect (sensor, endpoint) == 0);
    }

    //  Send out status messages to peers, and collect from peers
    //  The zmq_poll timeout defines our own heartbeating
    while (1) {
        //  Initialize poll set
        zmq_pollitem_t items [1] = {
            { sensor, 0, ZMQ_POLLIN, 0 }
        };
        assert (zmq_poll (items, 1, 1000000) >= 0);

        //  Handle incoming status message
        if (items [0].revents & ZMQ_POLLIN) {
            //  Get message key and discard it
            char *message_key = s_recv (sensor);
            //  Get address of peer broker
            char *broker_addr = s_recv (sensor);
            //  Get number of available workers
            char *broker_status = s_recv (sensor);

            printf ("%s - %s workers free\n", broker_addr, broker_status);
            free (message_key);
            free (broker_addr);
            free (broker_status);
        }
        else {
            //  Send message envelope and body
            //  Here, just some random value for worker availability
            char status [3];
            snprintf (status, 2, "%d", within (10));
            s_sendmore (tweeter, "STATUS");
            s_sendmore (tweeter, self);
            s_send     (tweeter, status);
        }
    }
    zmq_term (context);
    return 0;
}
