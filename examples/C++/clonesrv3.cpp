#include "kvsimple.hpp"

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

int main(void) {
    //  Prepare our context and sockets
    zmq::context_t ctx(1);
    zmq::socket_t snapshot(ctx, ZMQ_ROUTER);
    snapshot.bind("tcp://*:5556");
    zmq::socket_t publisher(ctx, ZMQ_PUB);
    publisher.bind("tcp://*:5557");
    zmq::socket_t collector(ctx, ZMQ_PULL);
    collector.bind("tcp://*:5558");

    //  .split body of main task
    //  The body of the main task collects updates from clients and
    //  publishes them back out to clients:
    std::unordered_map<std::string, kvmsg> kvmap;
    int64_t sequence = 0;

    zmq::pollitem_t items[] = {
        {collector, 0, ZMQ_POLLIN, 0},
        {snapshot, 0, ZMQ_POLLIN, 0}
    };

    s_catch_signals();
    while(!s_interrupted) {
        try {
            zmq::poll(items, 2, -1);
        } catch (const zmq::error_t& e) {
            break; //  Interrupted
        }
        //  Apply state update sent from client
        if (items[0].revents & ZMQ_POLLIN) {
            auto msg = kvmsg::recv(collector);
            if (!msg) {
                break; //  Interrupted
            }
            msg->set_sequence(++sequence);
            kvmap[msg->key()] = *msg;
            msg->send(publisher);
            std::cout << "I: publishing update " << sequence << std::endl;
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
            std::cout << "I: sending state snapshot=" << sequence << std::endl;
            s_sendmore(snapshot, identity);
            kvmsg msg("KTHXBAI", sequence, (unsigned char *)"");
            msg.send(snapshot);
        }
    }
    std::cout << "Interrupted\n" << sequence << " messages handled\n";
    return 0;
}