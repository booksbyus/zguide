//
//  Simple Pirate queue
//  This is identical to the LRU pattern, with no reliability mechanisms
//  at all. It depends on the client for recovery. Runs forever.
//
//  Andreas Hoelzlwimmer <andreas.hoelzlwimmer@fh-hagenberg.at
#include "zmsg.hpp"
#include <queue>

#define MAX_WORKERS 100

int main (void)
{
    s_version_assert (2, 1);

    //  Prepare our context and sockets
    zmq::context_t context(1);
    zmq::socket_t frontend (context, ZMQ_ROUTER);
    zmq::socket_t backend  (context, ZMQ_ROUTER);
    frontend.bind("tcp://*:5555");    //  For clients
    backend.bind("tcp://*:5556");     //  For workers

    //  Queue of available workers
    std::queue<std::string> worker_queue;

    while (1) {
        zmq::pollitem_t items [] = {
            { backend, 0, ZMQ_POLLIN, 0 },
            { frontend, 0, ZMQ_POLLIN, 0 }
        };
        //  Poll frontend only if we have available workers
        if (worker_queue.size())
            zmq::poll (items, 2, -1);
        else
            zmq::poll (items, 1, -1);

        //  Handle worker activity on backend
        if (items [0].revents & ZMQ_POLLIN) {
            zmsg zm(backend);
            //zmsg_t *zmsg = zmsg_recv (backend);

            //  Use worker address for LRU routing
            assert (worker_queue.size() < MAX_WORKERS);
            worker_queue.push(zm.unwrap());

            //  Return reply to client if it's not a READY
            if (strcmp (zm.address(), "READY") == 0)
                zm.clear();
            else
                zm.send (frontend);
        }
        if (items [1].revents & ZMQ_POLLIN) {
            //  Now get next client request, route to next worker
            zmsg zm(frontend);
            //  REQ socket in worker needs an envelope delimiter
            zm.wrap(worker_queue.front().c_str(), "");
            zm.send(backend);

            //  Dequeue and drop the next worker address
            worker_queue.pop();
        }
    }
    //  We never exit the main loop
    return 0;
}
