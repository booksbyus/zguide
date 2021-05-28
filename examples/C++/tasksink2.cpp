//
//  Task sink in C++ - design 2
//  Adds pub-sub flow to send kill signal to workers
//

#include "zhelpers.hpp"

int main (int argc, char *argv[])
{
    zmq::context_t context(1);

    //  Socket to receive messages on
    zmq::socket_t receiver (context, ZMQ_PULL);
    receiver.bind("tcp://*:5558");

    //  Socket for worker control
    zmq::socket_t controller (context, ZMQ_PUB);
    controller.bind("tcp://*:5559");

    //  Wait for start of batch
    s_recv (receiver);

    //  Start our clock now
    struct timeval tstart;
    gettimeofday (&tstart, NULL);

    //  Process 100 confirmations
    int task_nbr;
    for (task_nbr = 0; task_nbr < 100; task_nbr++) {
        s_recv (receiver);

        if (task_nbr % 10 == 0)
            std::cout << ":" ;
        else
            std::cout << "." ;
    }
    //  Calculate and report duration of batch
    struct timeval tend, tdiff;
    gettimeofday (&tend, NULL);

    if (tend.tv_usec < tstart.tv_usec) {
        tdiff.tv_sec = tend.tv_sec - tstart.tv_sec - 1;
        tdiff.tv_usec = 1000000 + tend.tv_usec - tstart.tv_usec;
    }
    else {
        tdiff.tv_sec = tend.tv_sec - tstart.tv_sec;
        tdiff.tv_usec = tend.tv_usec - tstart.tv_usec;
    }
    int total_msec = tdiff.tv_sec * 1000 + tdiff.tv_usec / 1000;
    std::cout << "\nTotal elapsed time: " << total_msec 
            << " msec\n" << std::endl;

    //  Send kill signal to workers
    s_send (controller, std::string("KILL"));

    //  Finished
    sleep (1);              //  Give 0MQ time to deliver
    return 0;
}
