#include "kvsimple.hpp"

int main(void) {
    zmq::context_t ctx(1);
    zmq::socket_t snapshot(ctx, ZMQ_DEALER);
    snapshot.connect("tcp://localhost:5556");
    zmq::socket_t subscriber(ctx, ZMQ_SUB);
    subscriber.set(zmq::sockopt::subscribe, "");
    subscriber.connect("tcp://localhost:5557");

    std::unordered_map<std::string, kvmsg> kvmap;

    //  Get state snapshot
    int64_t sequence = 0;
    s_send(snapshot, std::string("ICANHAZ?"));
    while (true) {
        auto kv = kvmsg::recv(snapshot);
        if (!kv) break;
        if (kv->key() == "KTHXBAI") {
            sequence = kv->sequence();
            std::cout << "Received snapshot=" << sequence << std::endl;
            break;
        }
        kvmap[kv->key()] = *kv;
    }
    //  Now apply pending updates, discard out-of-sequence messages
    while(true) {
        auto kv = kvmsg::recv(subscriber);
        if (!kv) break;
        if (kv->sequence() > sequence) {
            sequence = kv->sequence();
            kvmap[kv->key()] = *kv;
            std::cout << "Received update=" << sequence << std::endl;
        }
    }
    return 0;
}