//
//  Simple Pirate worker
//  Connects REQ socket to tcp://*:5556
//  Implements worker part of LRU queueing
//
#include "zmsg.h"

int main (void)
{
    srandom ((unsigned) time (NULL));

    void *context = zmq_init (1);
    void *worker = zmq_socket (context, ZMQ_REQ);

    //  Set random identity to make tracing easier
    char identity [10];
    sprintf (identity, "%04X-%04X", randof (0x10000), randof (0x10000));
    zmq_setsockopt (worker, ZMQ_IDENTITY, identity, strlen (identity));
    zmq_connect (worker, "tcp://localhost:5556");

    //  Tell queue we're ready for work
    printf ("I: (%s) worker ready\n", identity);
    s_send (worker, "READY");

    int cycles = 0;
    while (1) {
        zmsg_t *zmsg = zmsg_recv (worker);

        //  Simulate various problems, after a few cycles
        cycles++;
        if (cycles > 3 && randof (5) == 0) {
            printf ("I: (%s) simulating a crash\n", identity);
            zmsg_destroy (&zmsg);
            break;
        }
        else
        if (cycles > 3 && randof (5) == 0) {
            printf ("I: (%s) simulating CPU overload\n", identity);
            sleep (5);
        }
        printf ("I: (%s) normal reply - %s\n", 
                identity, zmsg_body (zmsg));
        sleep (1);              //  Do some heavy work
        zmsg_send (&zmsg, worker);
    }
    zmq_close (worker);
    zmq_term (context);
    return 0;
}
