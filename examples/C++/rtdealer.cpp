//
//  Custom routing Router to Dealer
//
// Olivier Chamoux <olivier.chamoux@fr.thalesgroup.com>

#include "zhelpers.hpp"

//  We have two workers, here we copy the code, normally these would
//  run on different boxes...
//
void *worker_a (void *arg) {
	
	zmq::context_t * context = (zmq::context_t *)arg;
    zmq::socket_t worker (*context, ZMQ_DEALER);
    worker.setsockopt( ZMQ_IDENTITY, "A", 1);
    worker.connect("ipc://routing.ipc");

    int total = 0;
    while (1) {
        //  We receive one part, with the workload
        std::string request = s_recv (worker);
        int finished = (request.compare("END") == 0);
        if (finished) {
            std::cout <<"A received: " << total << std::endl;
            break;
        }
        total++;
    }
    return (NULL);
}

void *worker_b (void *arg) {
	
	zmq::context_t * context = (zmq::context_t *)arg;
    zmq::socket_t worker (*context, ZMQ_DEALER);
    worker.setsockopt( ZMQ_IDENTITY, "B", 1);
    worker.connect("ipc://routing.ipc");

    int total = 0;
    while (1) {
        //  We receive one part, with the workload
        std::string request = s_recv (worker);
        int finished = (request.compare("END") == 0);
        if (finished) {
            std::cout <<"B received: " << total << std::endl;
            break;
        }
        total++;
    }
    return (NULL);
}

int main () {

    zmq::context_t context(1);

    zmq::socket_t client (context, ZMQ_ROUTER);
    client.bind("ipc://routing.ipc");

    pthread_t worker;
    pthread_create (&worker, NULL, worker_a, &context);
    pthread_create (&worker, NULL, worker_b, &context);

    //  Wait for threads to stabilize
    sleep (1);

    //  Send 10 tasks scattered to A twice as often as B
    int task_nbr;
    srandom ((unsigned) time (NULL));
    for (task_nbr = 0; task_nbr < 10; task_nbr++) {
        //  Send two message parts, first the address...
        if (within (3) > 0)
            s_sendmore (client, "A");
        else
            s_sendmore (client, "B");

        //  And then the workload
        s_send (client, "This is the workload");
    }
    s_sendmore (client, "A");
    s_send     (client, "END");

    s_sendmore (client, "B");
    s_send     (client, "END");

    sleep (1);              //  Give 0MQ/2.0.x time to flush output
    return 0;
}
