//
//  Paranoid Pirate worker
//
#include "zhelpers.h"
#include "zmsg.c"

#define HEARTBEAT_LIVENESS  3       //  3-5 is reasonable
#define HEARTBEAT_INTERVAL  1000    //  msecs
#define INTERVAL_INIT       1000    //  Initial reconnect
#define INTERVAL_MAX        1000   //  After exponential backoff

//  Helper function that returns a new configured socket
//  connected to the Hello World server
//
static char identity [10];

static void *
s_worker_socket (void *context) {
    void *worker = zmq_socket (context, ZMQ_XREQ);

    //  Set random identity to make tracing easier
    sprintf (identity, "%04X-%04X", randof (0x10000), randof (0x10000));
    zmq_setsockopt (worker, ZMQ_IDENTITY, identity, strlen (identity));
    zmq_connect (worker, "tcp://localhost:5556");

    //  Configure socket to not wait at close time
    int linger = 0;
    zmq_setsockopt (worker, ZMQ_LINGER, &linger, sizeof (linger));

    //  Tell queue we're ready for work
    printf ("I: (%s) worker ready\n", identity);
    s_send (worker, "READY");

    return worker;
}

int main (void)
{
    s_version_assert (2, 1);
    srandom ((unsigned) time (NULL));

    void *context = zmq_init (1);
    void *worker = s_worker_socket (context);

    //  If liveness hits zero, queue is considered disconnected
    size_t liveness = HEARTBEAT_LIVENESS;
    size_t interval = INTERVAL_INIT;

    //  Send out heartbeats at regular intervals
    uint64_t heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;

    int hbcount = 0;
    int cycles = 0;
    while (1) {
        zmq_pollitem_t items [] = { { worker,  0, ZMQ_POLLIN, 0 } };
        zmq_poll (items, 1, HEARTBEAT_INTERVAL * 1000);

        if (items [0].revents & ZMQ_POLLIN) {
            //  Get message
            //  - 3-part envelope + content -> request
            //  - 1-part "HEARTBEAT" -> heartbeat
            zmsg_t *zmsg = zmsg_recv (worker);

            if (zmsg_parts (zmsg) == 3) {
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
                zmsg_send (&zmsg, worker);
                liveness = HEARTBEAT_LIVENESS;
                sleep (1);              //  Do some heavy work
            }
            else
            if (zmsg_parts (zmsg) == 1
            && strcmp (zmsg_body (zmsg), "HEARTBEAT") == 0)
                liveness = HEARTBEAT_LIVENESS;
            else {
                printf ("E: (%s) invalid message\n", identity);
                zmsg_dump (zmsg);
            }
            interval = INTERVAL_INIT;
        }
        else
        if (--liveness == 0) {
            printf ("W: (%s) heartbeat failure, can't reach queue\n",
                identity);
            printf ("W: (%s) reconnecting in %zd msec...\n",
                identity, interval);
            s_sleep (interval);

            if (interval < INTERVAL_MAX)
                interval *= 2;
            zmq_close (worker);
            worker = s_worker_socket (context);
            liveness = HEARTBEAT_LIVENESS;
        }

        //  Send heartbeat to queue if it's time
        if (s_clock () > heartbeat_at) {
            heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;
            printf ("I: (%s) worker heartbeat\n", identity);
            s_send (worker, "HEARTBEAT");
        }
    }
    zmq_close (worker);
    zmq_term (context);
    return 0;
}
