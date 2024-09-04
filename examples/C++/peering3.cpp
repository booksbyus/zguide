#include "zhelpers.hpp"
#include <thread>
#include <queue>
#include <vector>

#define NBR_CLIENTS 6
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

// Broker name
static std::string self;

//  .split client task
//  This is the client task. It issues a burst of requests and then
//  sleeps for a few seconds. This simulates sporadic activity; when
//  a number of clients are active at once, the local workers should
//  be overloaded. The client uses a REQ socket for requests and also
//  pushes statistics to the monitor socket:

void client_thread(int id) {
    zmq::context_t context(1);
    zmq::socket_t client(context, ZMQ_REQ);
    std::string connURL = std::string("ipc://").append(self).append("-localfe.ipc");
#if (defined(WIN32))
    s_set_id(client, id);
    client.connect(connURL); // localfe
#else
    s_set_id(client); // Set a printable identity
    client.connect(connURL);
#endif

    zmq::socket_t monitor(context, ZMQ_PUSH);
    std::string moniURL = std::string("ipc://").append(self).append("-monitor.ipc");
    monitor.connect(moniURL);

    while (true) {
        sleep(within(5));
        int burst = within(15);
        while (burst--) {
            char task_id[5];
            sprintf(task_id, "%04X", within(0x10000));

            //  Send request with random hex ID
            s_send(client, std::string(task_id));

            zmq_pollitem_t items[] = { { client, 0, ZMQ_POLLIN, 0 } };
            try{
                zmq::poll(items, 1, 10 * 1000 * ZMQ_POLL_MSEC); // 10 seconds timeout
            } catch (zmq::error_t& e) {
                std::cout << "client_thread: " << e.what() << std::endl;
                break;
            }
            if (items[0].revents & ZMQ_POLLIN) {
                std::string reply = s_recv(client);
                assert(reply == std::string(task_id));
                // Do not print directly, send to monitor
                s_send(monitor, reply);
            } else {
                std::string reply = "E: CLIENT EXIT - lost task " + std::string(task_id);
                s_send(monitor, reply);
                return;
            }
        }
    }
}

//  .split worker task
//  This is the worker task, which uses a REQ socket to plug into the
//  load-balancer. It's the same stub worker task that you've seen in 
//  other examples:

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
        //  Workers are busy for 0/1 seconds
        sleep(within(2));
        send_all_frames(worker, frames);
    }
    return;
}

//  .split main task
//  The main task begins by setting up all its sockets. The local frontend
//  talks to clients, and our local backend talks to workers. The cloud
//  frontend talks to peer brokers as if they were clients, and the cloud
//  backend talks to peer brokers as if they were workers. The state
//  backend publishes regular state messages, and the state frontend
//  subscribes to all state backends to collect these messages. Finally,
//  we use a PULL monitor socket to collect printable messages from tasks:

int main(int argc, char *argv []) {
    // First argument is this broker's name
    // Other arguments are our peers' names
    if (argc < 2) {
        std::cout << "syntax: peering3 me {you} ..." << std::endl;
        return 0;
    }

    self = std::string(argv[1]);

    std::cout << "I: preparing broker at " << self << " ..." << std::endl;
    srandom(static_cast<unsigned int>(time(nullptr)));

    zmq::context_t context(1);
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

    //  Bind cloud frontend to endpoint
    zmq::socket_t cloudfe(context, ZMQ_ROUTER);
    cloudfe.set(zmq::sockopt::routing_id, self);
    std::string bindURL = std::string("ipc://").append(self).append("-cloud.ipc");
    cloudfe.bind(bindURL);

    // Connect cloud backend to all peers
    zmq::socket_t cloudbe(context, ZMQ_ROUTER);
    cloudbe.set(zmq::sockopt::routing_id, self);
    for(int argn = 2 ; argn < argc ; ++argn) {
        std::string peer(argv[argn]);
        std::cout << "I: connecting to cloud frontend at " <<  peer << std::endl;
        std::string peerURL = std::string("ipc://").append(peer).append("-cloud.ipc");
        cloudbe.connect(peerURL); // 将自己的cloudbe连接到其他broker的cloudfe
    }

    // Bind state backend to endpoint
    zmq::socket_t statebe(context, ZMQ_PUB);
    {
        std::string bindURL = std::string("ipc://").append(self).append("-state.ipc");
        statebe.bind(bindURL);
    }

    // Connect statefe to all peers
    zmq::socket_t statefe(context, ZMQ_SUB);
    statefe.set(zmq::sockopt::subscribe, "");
    for(int argn = 2 ; argn < argc ; ++argn) {
        std::string peer(argv[argn]);
        std::string peerURL = std::string("ipc://").append(peer).append("-state.ipc");
        statefe.connect(peerURL);
    }

    // Prepare monitor socket
    zmq::socket_t monitor(context, ZMQ_PULL);
    std::string moniURL = std::string("ipc://").append(self).append("-monitor.ipc");
    monitor.bind(moniURL);

    //  .split start child tasks
    //  After binding and connecting all our sockets, we start our child
    //  tasks - workers and clients:

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

    // Queue of available workers
    int local_capacity = 0;
    int cloud_capacity = 0;
    std::queue<std::string> workers;

    //  .split main loop
    //  The main loop has two parts. First, we poll workers and our two service
    //  sockets (statefe and monitor), in any case. If we have no ready workers,
    //  then there's no point in looking at incoming requests. These can remain 
    //  on their internal 0MQ queues:
    while (true) {
        zmq_pollitem_t primary [] = {
            {localbe, 0, ZMQ_POLLIN, 0},
            {cloudbe, 0, ZMQ_POLLIN, 0},
            {statefe, 0, ZMQ_POLLIN, 0},
            {monitor, 0, ZMQ_POLLIN, 0}
        };
        try {
            //  If we have no workers ready, wait indefinitely
            std::chrono::milliseconds timeout{(local_capacity ? 1000 * ZMQ_POLL_MSEC : -1)};
            zmq::poll(primary, 4, timeout);
        } catch(...) {
            break;
        }

        // Track if capacity changes during this iteration
        int previous = local_capacity;

        if (primary[0].revents & ZMQ_POLLIN) {
            // From localbe, reply from local worker
            std::string worker_identity = s_recv(localbe);
            workers.push(worker_identity);
            local_capacity++;
            receive_empty_message(localbe);

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
        } else if (primary[1].revents & ZMQ_POLLIN) {
            // From cloudbe，handle reply from peer broker
            std::string peer_broker_identity = s_recv(cloudbe); // useless
            receive_empty_message(cloudbe);
            std::string client_addr = s_recv(cloudbe);
            receive_empty_message(cloudbe);
            std::string reply = s_recv(cloudbe);
            // send to the client
            s_sendmore(localfe, client_addr);
            s_sendmore(localfe, std::string(""));
            s_send(localfe, reply);
        }
        //  .split handle state messages
        //  If we have input messages on our statefe or monitor sockets, we
        //  can process these immediately:
        if (primary[2].revents & ZMQ_POLLIN) {
            // From statefe, receive other brokers state
            std::string peer(s_recv(statefe));
            std::string status(s_recv(statefe));
            cloud_capacity = atoi(status.c_str());
        }
        if (primary[3].revents & ZMQ_POLLIN) {
            // From monitor, receive printable message
            std::string message(s_recv(monitor));
            std::cout << "monitor: " << message << std::endl;
        }
        //  .split route client requests
        //  Now route as many clients requests as we can handle. If we have
        //  local capacity, we poll both localfe and cloudfe. If we have cloud
        //  capacity only, we poll just localfe. We route any request locally
        //  if we can, else we route to the cloud.
        while (local_capacity + cloud_capacity) {
            zmq_pollitem_t secondary [] = {
                {localfe, 0, ZMQ_POLLIN, 0},
                {cloudfe, 0, ZMQ_POLLIN, 0}
            };
            if (local_capacity) {
                try {
                    zmq::poll(secondary, 2, 0);
                } catch(...) {
                    break;
                }
            } else {
                try {
                    zmq::poll(secondary, 1, 0);
                } catch(...) {
                    break;
                }
            }
            std::vector<std::string> msg;
            if (secondary[0].revents & ZMQ_POLLIN) {
                // From localfe, receive client request
                receive_all_frames(localfe, msg);
            } else if (secondary[1].revents & ZMQ_POLLIN) {
                // From cloudfe, receive other broker's request
                receive_all_frames(cloudfe, msg);
            } else {
                break;
            }

            if (local_capacity) {
                //  Route to local worker
                std::string worker_addr = workers.front();
                workers.pop();
                local_capacity--;
                s_sendmore(localbe, worker_addr);
                s_sendmore(localbe, std::string(""));
                send_all_frames(localbe, msg);
            } else {
                //  Route to cloud
                int peer = within(argc - 2) + 2;
                s_sendmore(cloudbe, std::string(argv[peer]));
                s_sendmore(cloudbe, std::string(""));
                send_all_frames(cloudbe, msg);
            }
        }
        //  .split broadcast capacity
        //  We broadcast capacity messages to other peers; to reduce chatter,
        //  we do this only if our capacity changed.
        if (local_capacity != previous) {
            std::ostringstream intStream;
            intStream << local_capacity;
            s_sendmore(statebe, self);
            s_send(statebe, intStream.str());
        }
    }
    return 0;
}