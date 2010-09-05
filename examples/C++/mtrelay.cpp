//
//  Multithreaded relay in C++
//
//  Olivier Chamoux <olivier.chamoux@fr.thalesgroup.com>
//
#include <zmq.hpp>
#include <pthread.h>
#include <iostream>

void *step1 (void *arg) {
    zmq::context_t *context = static_cast<zmq::context_t*>(arg);

    //  Signal downstream to step 2
    zmq::socket_t sender (*context, ZMQ_PAIR);
    sender.connect("inproc://step2");
    zmq::message_t message;
    sender.send(message);

    return (NULL);
}

//  Step 2 relays the signal to step 3

void *step2 (void *arg) {
    zmq::context_t *context = static_cast<zmq::context_t*>(arg);

    //  Bind to inproc: endpoint, then start upstream thread
    zmq::socket_t receiver (*context, ZMQ_PAIR);
    receiver.bind("inproc://step2");
    pthread_t thread;
    pthread_create (&thread, NULL, step1, context);

    //  Wait for signal
    zmq::message_t message;
    receiver.recv(&message);

    //  Signal downstream to step 3
    zmq::socket_t sender (*context, ZMQ_PAIR);
    sender.connect("inproc://step3");
    sender.send(message);

    return (NULL);
}

int main () {
    zmq::context_t context(1);

    //  Bind to inproc: endpoint, then start upstream thread
    zmq::socket_t receiver (context, ZMQ_PAIR);
    receiver.bind("inproc://step3");
    pthread_t thread;
    pthread_create (&thread, NULL, step2, &context);

    //  Wait for signal
    zmq::message_t message;
    receiver.recv(&message);

    std::cout << "Test successful!" << std::endl;
    return 0;
}
