#include "kvsimple.hpp"
#include <thread>

static int s_send_snapshot(std::unordered_map<std::string, kvmsg>& kvmap, zmq::socket_t* snapshot);
static void state_manager(zmq::context_t* ctx);

// simulate zthread_fork, create attached thread and return the pipe socket
std::pair<std::thread, zmq::socket_t> zthread_fork(zmq::context_t& ctx, void (*thread_func)(zmq::context_t*)) {
    // create the pipe socket for the main thread to communicate with its child thread
    zmq::socket_t pipe(ctx, ZMQ_PAIR);
    pipe.connect("inproc://state_manager");

    // start child thread
    std::thread t(thread_func, &ctx);

    return std::make_pair(std::move(t), std::move(pipe));
}

int main(void) {
    // Prepare our context and socket
    zmq::context_t ctx(1);
    zmq::socket_t publisher(ctx, ZMQ_PUB);
    publisher.bind("tcp://*:5557");

    int64_t sequence = 0;

    //  Start state manager and wait for synchronization signal
    auto [state_manager_thread, state_manager_pipe] = zthread_fork(ctx, state_manager);
    zmq::message_t sync_msg;
    state_manager_pipe.recv(sync_msg);

    s_catch_signals();
    while(!s_interrupted) {
        kvmsg msg = kvmsg("key", ++sequence, (unsigned char *)"value");
        msg.set_key(std::to_string(within(10000)));
        msg.set_body((unsigned char *)std::to_string(within(1000000)).c_str());
        msg.send(publisher);
        msg.send(state_manager_pipe);
        s_sleep(500);
    }
    std::cout << " Interrupted\n" <<  sequence << " messages out\n" << std::endl;
    kvmsg msg("END", sequence, (unsigned char *)"");
    msg.send(state_manager_pipe); 
    state_manager_thread.join();
    return 0;
}

//  Routing information for a key-value snapshot
typedef struct {
    zmq::socket_t *socket; //  ROUTER socket to send to
    std::string identity; //  Identity of peer who requested state
} kvroute_t;

//  Send one state snapshot key-value pair to a socket
//  Hash item data is our kvmsg object, ready to send
static int s_send_snapshot(std::unordered_map<std::string, kvmsg>& kvmap, kvroute_t& kvroute) {
    for (auto& kv : kvmap) {
        s_sendmore(*kvroute.socket, kvroute.identity);
        kv.second.send(*kvroute.socket);
    }
    return 0;
}

//  .split state manager
//  The state manager task maintains the state and handles requests from
//  clients for snapshots:
static void state_manager(zmq::context_t *ctx) {
    std::unordered_map<std::string, kvmsg> kvmap;
    zmq::socket_t pipe(*ctx, ZMQ_PAIR);
    pipe.bind("inproc://state_manager");
    s_send(pipe, std::string("READY"));

    zmq::socket_t snapshot(*ctx, ZMQ_ROUTER);
    snapshot.bind("tcp://*:5556");

    zmq::pollitem_t items[] = {
        {pipe, 0, ZMQ_POLLIN, 0},
        {snapshot, 0, ZMQ_POLLIN, 0}
    };
    int64_t sequence = 0;
    while(true) {
        zmq::poll(&items[0], 2, -1);
        if (items[0].revents & ZMQ_POLLIN) {
            auto msg = kvmsg::recv(pipe);
            if (!msg || msg->key() == "END") {
                break;
            }
            sequence = msg->sequence();
            kvmap[msg->key()] = *msg;
        }
        //  Execute state snapshot request
        if (items[1].revents & ZMQ_POLLIN) {
            std::string identity = s_recv(snapshot);
            std::string request = s_recv(snapshot);
            if (request != "ICANHAZ?") {
                std::cerr << "E: bad request, aborting\n";
                break;
            }
            //  Send state snapshot to client
            kvroute_t kvroute = {&snapshot, identity};

            //  For each entry in kvmap, send kvmsg to client
            s_send_snapshot(kvmap, kvroute);

            //  Now send END message with sequence number
            std::cout << "sending state snapshot=" << sequence << std::endl;
            s_sendmore(snapshot, identity);
            kvmsg msg("KTHXBAI", sequence, (unsigned char *)"");
            msg.send(snapshot);
        }
    }
}