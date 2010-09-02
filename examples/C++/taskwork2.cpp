//
//  Task worker in C++ - design 2
//  Adds pub-sub flow to receive and respond to kill signal
//
//  Olivier Chamoux <olivier.chamoux@fr.thalesgroup.com>
//
#include <zmq.hpp>
#include <time.h>
#include <sstream>
#include <iostream>

int main (int argc, char *argv[])
{
    zmq::context_t context(1);

    //  Socket to receive messages on
    zmq::socket_t receiver(context, ZMQ_PULL);
    receiver.connect("tcp://localhost:5557");

    //  Socket to send messages to
    zmq::socket_t sender(context, ZMQ_PUSH);
    sender.connect("tcp://localhost:5558");

    //  Socket for control input
    zmq::socket_t controller (context, ZMQ_SUB);
    controller.connect("tcp://localhost:5559");
   	controller.setsockopt(ZMQ_SUBSCRIBE, "", 0);

    //  Process messages from receiver and controller
    zmq::pollitem_t items [2] = {
        { receiver, 0, ZMQ_POLLIN, 0 },
        { controller, 0, ZMQ_POLLIN, 0 }
    };
    //  Process messages from both sockets
    while (1) {
        zmq::message_t message;
        zmq::poll (&items [0], 2, -1);

        if (items [0].revents & ZMQ_POLLIN) {
			receiver.recv(&message);

            //  Process task
            int workload;           //  Workload in msecs
            struct timespec t;

            std::istringstream iss(static_cast<char*>(message.data()));
            iss >> workload;

            t.tv_sec = 0;
            t.tv_nsec = workload * 1000000;

            //  Do the work
            nanosleep (&t, NULL);

            //  Send results to sink
            message.rebuild();
            sender.send(message);

            //  Simple progress indicator for the viewer
            std::cout << "." << std::flush;

        }
        //  Any waiting controller command acts as 'KILL'
        if (items [1].revents & ZMQ_POLLIN)
        {
        	std::cout << std::endl;
            break;                      //  Exit loop
    	}
    }
    //  Finished
    return 0;
}
