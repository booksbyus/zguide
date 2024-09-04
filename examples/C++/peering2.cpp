//
// created by Jinyang Shao on 8/22/2024
//

//  Broker peering simulation (part 2)
//  Prototypes the request-reply flow

#include "zhelpers.hpp"
#include <thread>
#include <queue>
#include <vector>

#define NBR_CLIENTS 10
#define NBR_WORKERS 3
#define WORKER_READY   "\001"      //  Signals worker is ready
#define ZMQ_POLL_MSEC 1


void receive_all_frames(zmq::socket_t& sock, std::vector<std::string>& frames) {
    frames.clear();
    while (1) {
        //  Process all parts of the message
        std::string frame = s_recv(sock);
        frames.emplace_back(frame);
        int more = 0;           //  Multipart detection
        size_t more_size = sizeof (more);
        sock.getsockopt(ZMQ_RCVMORE, &more, &more_size);
        if (!more)
            break;              //  Last message part
    }
    return;
}

void send_all_frames(zmq::socket_t& sock, std::vector<std::string>& frames) {
    for (int i = 0; i < frames.size(); i++) {
        if (i == frames.size() - 1) {
            s_send(sock, frames[i]);
        } else {
            s_sendmore(sock, frames[i]);
        }
    }
    return;
}

void receive_empty_message(zmq::socket_t& sock)
{
    std::string empty = s_recv(sock);
    assert(empty.size() == 0);
}

void print_all_frames(std::vector<std::string>& frames) {
    std::cout << "------------received------------" << std::endl;
    for (std::string &frame : frames)
    {
        std::cout << frame << std::endl;
        std::cout << "----------------------------------------" << std::endl;
    }
}

// Broker's identity
static std::string self;

void client_thread(int id) {
    zmq::context_t context(1);
    zmq::socket_t client(context, ZMQ_REQ);

    std::string connURL = std::string("ipc://").append(self).append("-localfe.ipc");
#if (defined (WIN32))
    s_set_id(client, id);
    client.connect(connURL); // localfe
#else
    s_set_id(client); // Set a printable identity
    client.connect(connURL);
#endif
    while(true) {
        // Send request, get reply
        s_send(client, std::string("HELLO"));
        std::string reply = s_recv(client);
        std::cout << "Client" << reply << std::endl;
        sleep(1);
    }
    return;
}

//  Worker using REQ socket to do LRU routing
//
void worker_thread(int id) {
    zmq::context_t context(1);
    zmq::socket_t worker(context, ZMQ_REQ);

    std::string connURL = std::string("ipc://").append(self).append("-localbe.ipc");
#if (defined (WIN32))
    s_set_id(worker, id);
    worker.connect(connURL); // backend
#else
    s_set_id(worker);
    worker.connect(connURL);
#endif

    //  Tell broker we're ready for work
    s_send(worker, std::string(WORKER_READY));

    while (true) {
        //  Read and save all frames until we get an empty frame
        //  In this example there is only 1 but it could be more
        std::vector<std::string> frames;
        receive_all_frames(worker, frames);
        std::cout << "Worker: " << frames[frames.size()-1] << std::endl;
        // Send reply
        frames[frames.size()-1] = std::string("OK");
        send_all_frames(worker, frames);
    }
    return;
}

int main(int argc, char *argv[]) {
    // First argument is this broker's name
    // Other arguments are our peers' names

    if (argc < 2) {
        std::cout << "syntax: peering2 me {you} ..." << std::endl;
        return 0;
    }

    self = std::string(argv[1]);

    std::cout << "I: preparing broker at " << self << " ..." << std::endl;
    srandom(static_cast<unsigned int>(time(nullptr)));

    zmq::context_t context(1);

    //  Bind cloud frontend to endpoint
    zmq::socket_t cloudfe(context, ZMQ_ROUTER);
    cloudfe.set(zmq::sockopt::routing_id, self); // remember to set identity
    std::string bindURL = std::string("ipc://").append(self).append("-cloud.ipc");
    cloudfe.bind(bindURL);

    // Connect cloud backend to all peers
    zmq::socket_t cloudbe(context, ZMQ_ROUTER);
    cloudbe.set(zmq::sockopt::routing_id, self); // remember to set identity
    for(int argn = 2 ; argn < argc ; ++argn) {
        std::string peer(argv[argn]);
        std::cout << "I: connecting to cloud frontend at " <<  peer << std::endl;
        std::string peerURL = std::string("ipc://").append(peer).append("-cloud.ipc");
        cloudbe.connect(peerURL);
    }

    // Prepare local frontend and backend
    zmq::socket_t localfe(context, ZMQ_ROUTER);
    {
        std::string bindURL = std::string("ipc://").append(self).append("-localfe.ipc");
        localfe.bind(bindURL);
    }
    zmq::socket_t localbe(context, ZMQ_ROUTER);
    {
        std::string bindURL = std::string("ipc://").append(self).append("-localbe.ipc");
        localbe.bind(bindURL);
    }
    
    //  Get user to tell us when we can start...
    std::cout << "Press Enter when all brokers are started: " << std::endl;
    getchar();


    // Start local clients
    int client_nbr = 0;
    for (; client_nbr < NBR_CLIENTS; client_nbr++)
    {
        std::thread t(client_thread, client_nbr);
        t.detach();
    }
    // Start local workers
    for (int worker_nbr = 0; worker_nbr < NBR_WORKERS; worker_nbr++)
    {
        std::thread t(worker_thread, worker_nbr);
        t.detach();
    }

    // Interesting part
    //  .split request-reply handling
    //  Here, we handle the request-reply flow. We're using load-balancing
    //  to poll workers at all times, and clients only when there are one
    //  or more workers available.

    //  Least recently used queue of available workers
    int capacity = 0;
    std::queue<std::string> worker_queue;

    zmq::pollitem_t frontend_items[] = {
        {localfe, 0, ZMQ_POLLIN, 0},
        {cloudfe, 0, ZMQ_POLLIN, 0}
    };

    zmq::pollitem_t backend_items[] = {
        {localbe, 0, ZMQ_POLLIN, 0},
        {cloudbe, 0, ZMQ_POLLIN, 0}
    };
    while(true) {
        //  First, route any waiting replies from workers

        try {
            //  If we have no workers, wait indefinitely
            std::chrono::milliseconds timeout{(capacity ? 1000 : -1)};
            zmq::poll(backend_items, 2, timeout);
        } catch(...) {
            break;
        }

        if (backend_items[0].revents & ZMQ_POLLIN) {
            // From localbe，Handle reply from local worker
            std::string worker_identity = s_recv(localbe);
            worker_queue.push(worker_identity);
            capacity++;
            receive_empty_message(localbe);
            
            // Remain_frames may be:
            // 1. [client_addr][0][OK]
            // 2. [origin_broker][0][client_addr][0][OK]
            // 3. [READY]
            std::vector<std::string> remain_frames;
            receive_all_frames(localbe, remain_frames);
            assert(remain_frames.size() == 1 || remain_frames.size() == 3 || remain_frames.size() == 5);
            //  Third frame is READY or else a client reply address
            std::string third_frame = remain_frames[0];

            //  If the third_frame is client_addr
            if (third_frame.compare(WORKER_READY) != 0 && remain_frames.size() == 3) {
                // Send to client
                send_all_frames(localfe, remain_frames);
            } else if (remain_frames.size() == 5) {
                // The third_frame is origin_broker address
                // Route the reply to the origin broker
                for (int argn = 2; argn < argc; argn++) {
                    if (third_frame.compare(argv[argn]) == 0) {
                        send_all_frames(cloudfe, remain_frames);
                    }
                }
            }
        } else if (backend_items[1].revents & ZMQ_POLLIN) {
            // From cloudbe，handle reply from peer broker
            std::string peer_broker_identity = s_recv(cloudbe); // useless
            receive_empty_message(cloudbe);
            std::string client_addr = s_recv(cloudbe);
            receive_empty_message(cloudbe);
            std::string reply = s_recv(cloudbe);
            // Send to the client
            s_sendmore(localfe, client_addr);
            s_sendmore(localfe, std::string(""));
            s_send(localfe, reply);
        }

        //  .split route client requests
        //  Now we route as many client requests as we have worker capacity
        //  for. We may reroute requests from our local frontend, but not from
        //  the cloud frontend. We reroute randomly now, just to test things
        //  out. In the next version, we'll do this properly by calculating
        //  cloud capacity:
        while (capacity){
            try{
                // No wait
                zmq::poll(frontend_items, 2, 0);
            }
            catch (...) {
                break;
            }

            bool reroutable = false; // not used in C++

            if (frontend_items[0].revents & ZMQ_POLLIN) {
                // From localfe, client's request
                std::string client_addr = s_recv(localfe);
                receive_empty_message(localfe);
                std::string request = s_recv(localfe);

                reroutable = true;
                // Route in 20% of cases
                if (argc > 2 && within(5) < 1) {
                    // Peers exist and routable
                    int peer = within(argc-2) + 2;
                    std::string peer_addr = argv[peer];
                    // Send to cloudbe, routing
                    s_sendmore(cloudbe, peer_addr);
                    s_sendmore(cloudbe, std::string(""));
                    s_sendmore(cloudbe, client_addr);
                    s_sendmore(cloudbe, std::string(""));
                    s_send(cloudbe, request);
                } else {
                    // Use local workers
                    std::string worker_addr = worker_queue.front();
                    worker_queue.pop();
                    capacity--;
                    // Send to local worker
                    s_sendmore(localbe, worker_addr);
                    s_sendmore(localbe, std::string(""));
                    s_sendmore(localbe, client_addr);
                    s_sendmore(localbe, std::string(""));
                    s_send(localbe, request);
                }
            } else if (frontend_items[1].revents & ZMQ_POLLIN) {
                // From cloudfe, other broker's request
                std::string origin_peer_addr = s_recv(cloudfe);
                receive_empty_message(cloudfe);
                std::string client_addr = s_recv(cloudfe);
                receive_empty_message(cloudfe);
                std::string request = s_recv(cloudfe);

                reroutable = false;
                // Use local workers
                std::string worker_addr = worker_queue.front();
                worker_queue.pop();
                capacity--;
                // Send to local worker
                s_sendmore(localbe, worker_addr);
                s_sendmore(localbe, std::string(""));
                s_sendmore(localbe, origin_peer_addr);
                s_sendmore(localbe, std::string(""));
                s_sendmore(localbe, client_addr);
                s_sendmore(localbe, std::string(""));
                s_send(localbe, request);
            } else {
                break; //  No work, go back to backends
            }
        }
    }
    return 0;
}