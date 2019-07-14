//
//  Paranoid Pirate worker
//
//
//     Andreas Hoelzlwimmer <andreas.hoelzlwimmer@fh-hagenberg.at>
//
#include "zmsg.hpp"

#include <iomanip>

#define HEARTBEAT_LIVENESS  3       //  3-5 is reasonable
#define HEARTBEAT_INTERVAL  1000    //  msecs
#define INTERVAL_INIT       1000    //  Initial reconnect
#define INTERVAL_MAX       32000    //  After exponential backoff

//  Helper function that returns a new configured socket
//  connected to the Hello World server
//
std::string identity;

static zmq::socket_t *
s_worker_socket (zmq::context_t &context) {
    zmq::socket_t * worker = new zmq::socket_t(context, ZMQ_DEALER);

    //  Set random identity to make tracing easier
    identity = s_set_id(*worker);
    worker->connect ("tcp://localhost:5556");

    //  Configure socket to not wait at close time
    int linger = 0;
    worker->setsockopt (ZMQ_LINGER, &linger, sizeof (linger));

    //  Tell queue we're ready for work
    std::cout << "I: (" << identity << ") worker ready" << std::endl;
    s_send (*worker, "READY");

    return worker;
}

int main (void)
{
    s_version_assert (4, 0);
    srandom ((unsigned) time (NULL));

    zmq::context_t context (1);
    zmq::socket_t * worker = s_worker_socket (context);

    //  If liveness hits zero, queue is considered disconnected
    size_t liveness = HEARTBEAT_LIVENESS;
    size_t interval = INTERVAL_INIT;

    //  Send out heartbeats at regular intervals
    int64_t heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;

    int cycles = 0;
    while (1) {
        zmq::pollitem_t items[] = {
            {static_cast<void*>(*worker), 0, ZMQ_POLLIN, 0 } };
        zmq::poll (items, 1, HEARTBEAT_INTERVAL);

        if (items [0].revents & ZMQ_POLLIN) {
            //  Get message
            //  - 3-part envelope + content -> request
            //  - 1-part "HEARTBEAT" -> heartbeat
            zmsg msg (*worker);

            if (msg.parts () == 3) {
                //  Simulate various problems, after a few cycles
                cycles++;
                if (cycles > 3 && within (5) == 0) {
                    std::cout << "I: (" << identity << ") simulating a crash" << std::endl;
                    msg.clear ();
                    break;
                }
                else {
                   if (cycles > 3 && within (5) == 0) {
                      std::cout << "I: (" << identity << ") simulating CPU overload" << std::endl;
                       sleep (5);
                   }
                }
                std::cout << "I: (" << identity << ") normal reply - " << msg.body() << std::endl;
                msg.send (*worker);
                liveness = HEARTBEAT_LIVENESS;
                sleep (1);              //  Do some heavy work
            }
            else {
               if (msg.parts () == 1
               && strcmp (msg.body (), "HEARTBEAT") == 0) {
                   liveness = HEARTBEAT_LIVENESS;
               }
               else {
                   std::cout << "E: (" << identity << ") invalid message" << std::endl;
                   msg.dump ();
               }
            }
            interval = INTERVAL_INIT;
        }
        else
        if (--liveness == 0) {
            std::cout << "W: (" << identity << ") heartbeat failure, can't reach queue" << std::endl;
            std::cout << "W: (" << identity << ") reconnecting in " << interval << " msec..." << std::endl;
            s_sleep (interval);

            if (interval < INTERVAL_MAX) {
                interval *= 2;
            }
            delete worker;
            worker = s_worker_socket (context);
            liveness = HEARTBEAT_LIVENESS;
        }

        //  Send heartbeat to queue if it's time
        if (s_clock () > heartbeat_at) {
            heartbeat_at = s_clock () + HEARTBEAT_INTERVAL;
            std::cout << "I: (" << identity << ") worker heartbeat" << std::endl;
            s_send (*worker, "HEARTBEAT");
        }
    }
    delete worker;
    return 0;
}
