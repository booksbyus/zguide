//
//  Simple Pirate worker
//  Connects REQ socket to tcp://*:5556
//  Implements worker part of LRU queueing
//
//  Andreas Hoelzlwimmer <andreas.hoelzlwimmer@fh-hagenberg.at>
#include "zmsg.hpp"

int main (void)
{
    srandom ((unsigned) time (NULL));

    zmq::context_t context(1);
    zmq::socket_t worker(context, ZMQ_REQ);

    //  Set random identity to make tracing easier
    std::string identity = s_set_id(worker);
    worker.connect("tcp://localhost:5556");

    //  Tell queue we're ready for work
    std::cout << "I: (" << identity << ") worker ready" << std::endl;
    s_send (worker, "READY");

    int cycles = 0;
    while (1) {
        zmsg zm (worker);

        //  Simulate various problems, after a few cycles
        cycles++;
        if (cycles > 3 && within (5) == 0) {
            std::cout << "I: (" << identity << ") simulating a crash" << std::endl;
            zm.clear ();
            break;
        }
        else
        if (cycles > 3 && within (5) == 0) {
            std::cout << "I: (" << identity << ") simulating CPU overload" << std::endl;
            sleep (5);
        }
        std::cout << "I: (" << identity << ") normal reply - " << zm.body () << std::endl;
        sleep (1);              //  Do some heavy work
        zm.send(worker);
    }
    return 0;
}
