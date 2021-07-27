//  Least-recently used (LRU) queue device
//  Clients and workers are shown here in-process
//

#include "zhelpers.hpp"
#include <pthread.h>
#include <queue>

//  Basic request-reply client using REQ socket
//
static void *
client_thread(void *arg) {
    zmq::context_t context(1);
    zmq::socket_t client(context, ZMQ_REQ);

#if (defined (WIN32))
    s_set_id(client, (intptr_t)arg);
    client.connect("tcp://localhost:5672"); // frontend
#else
    s_set_id(client); // Set a printable identity
    client.connect("ipc://frontend.ipc");
#endif

    //  Send request, get reply
    s_send(client, "HELLO");
    std::string reply = s_recv(client);
    std::cout << "Client: " << reply << std::endl;
    return (NULL);
}

//  Worker using REQ socket to do LRU routing
//
static void *
worker_thread(void *arg) {
    zmq::context_t context(1);
    zmq::socket_t worker(context, ZMQ_REQ);

#if (defined (WIN32))
    s_set_id(worker, (intptr_t)arg);
    worker.connect("tcp://localhost:5673"); // backend
#else
    s_set_id(worker);
    worker.connect("ipc://backend.ipc");
#endif

    //  Tell backend we're ready for work
    s_send(worker, "READY");

    while (1) {
        //  Read and save all frames until we get an empty frame
        //  In this example there is only 1 but it could be more
        std::string address = s_recv(worker);
        {
            std::string empty = s_recv(worker);
            assert(empty.size() == 0);
        }

        //  Get request, send reply
        std::string request = s_recv(worker);
        std::cout << "Worker: " << request << std::endl;

        s_sendmore(worker, address);
        s_sendmore(worker, "");
        s_send(worker, "OK");
    }
    return (NULL);
}

int main(int argc, char *argv[])
{

    //  Prepare our context and sockets
    zmq::context_t context(1);
    zmq::socket_t frontend(context, ZMQ_ROUTER);
    zmq::socket_t backend(context, ZMQ_ROUTER);

#if (defined (WIN32))
    frontend.bind("tcp://*:5672"); // frontend
    backend.bind("tcp://*:5673"); // backend
#else
    frontend.bind("ipc://frontend.ipc");
    backend.bind("ipc://backend.ipc");
#endif

    int client_nbr;
    for (client_nbr = 0; client_nbr < 10; client_nbr++) {
        pthread_t client;
        pthread_create(&client, NULL, client_thread, (void *)(intptr_t)client_nbr);
    }
    int worker_nbr;
    for (worker_nbr = 0; worker_nbr < 3; worker_nbr++) {
        pthread_t worker;
        pthread_create(&worker, NULL, worker_thread, (void *)(intptr_t)worker_nbr);
    }
    //  Logic of LRU loop
    //  - Poll backend always, frontend only if 1+ worker ready
    //  - If worker replies, queue worker as ready and forward reply
    //    to client if necessary
    //  - If client requests, pop next worker and send request to it
    //
    //  A very simple queue structure with known max size
    std::queue<std::string> worker_queue;

    while (1) {

        //  Initialize poll set
        zmq::pollitem_t items[] = {
                //  Always poll for worker activity on backend
                { backend, 0, ZMQ_POLLIN, 0 },
                //  Poll front-end only if we have available workers
                { frontend, 0, ZMQ_POLLIN, 0 }
        };
        if (worker_queue.size())
            zmq::poll(&items[0], 2, -1);
        else
            zmq::poll(&items[0], 1, -1);

        //  Handle worker activity on backend
        if (items[0].revents & ZMQ_POLLIN) {

            //  Queue worker address for LRU routing
            worker_queue.push(s_recv(backend));

            {
                //  Second frame is empty
                std::string empty = s_recv(backend);
                assert(empty.size() == 0);
            }

            //  Third frame is READY or else a client reply address
            std::string client_addr = s_recv(backend);

            //  If client reply, send rest back to frontend
            if (client_addr.compare("READY") != 0) {

                    {
                        std::string empty = s_recv(backend);
                        assert(empty.size() == 0);
                    }

                std::string reply = s_recv(backend);
                s_sendmore(frontend, client_addr);
                s_sendmore(frontend, "");
                s_send(frontend, reply);

                if (--client_nbr == 0)
                    break;
            }
        }
        if (items[1].revents & ZMQ_POLLIN) {

            //  Now get next client request, route to LRU worker
            //  Client request is [address][empty][request]
            std::string client_addr = s_recv(frontend);

            {
                std::string empty = s_recv(frontend);
                assert(empty.size() == 0);
            }

            std::string request = s_recv(frontend);

            std::string worker_addr = worker_queue.front();//worker_queue [0];
            worker_queue.pop();

            s_sendmore(backend, worker_addr);
            s_sendmore(backend, "");
            s_sendmore(backend, client_addr);
            s_sendmore(backend, "");
            s_send(backend, request);
        }
    }
    return 0;
}
