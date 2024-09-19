//  Clone client - Model Three
#include "kvsimple.hpp"

int main(void) {
    //  Prepare our context and subscriber
    zmq::context_t ctx(1);
    zmq::socket_t snapshot(ctx, ZMQ_DEALER);
    snapshot.connect("tcp://localhost:5556");
    zmq::socket_t subscriber(ctx, ZMQ_SUB);
    subscriber.set(zmq::sockopt::subscribe, "");
    subscriber.connect("tcp://localhost:5557");
    zmq::socket_t publisher(ctx, ZMQ_PUSH);
    publisher.connect("tcp://localhost:5558");

    std::unordered_map<std::string, kvmsg> kvmap;

    //  .split getting a state snapshot
    //  We first request a state snapshot:
    //  Get state snapshot
    int64_t sequence = 0;
    s_send(snapshot, std::string("ICANHAZ?"));
    while (true) {
        auto kv = kvmsg::recv(snapshot);
        if (!kv) break;
        if (kv->key() == "KTHXBAI") {
            sequence = kv->sequence();
            std::cout << "I: received snapshot=" << sequence << std::endl;
            break;
        }
        kvmap[kv->key()] = *kv;
    }
    //  .split processing state updates
    //  Now we wait for updates from the server and every so often, we
    //  send a random key-value update to the server:
    std::chrono::time_point<std::chrono::steady_clock> alarm = std::chrono::steady_clock::now() + std::chrono::seconds(1);
    s_catch_signals();
    while(!s_interrupted) {
        zmq::pollitem_t items[] = {
            {subscriber, 0, ZMQ_POLLIN, 0}
        };
        int tickless = std::chrono::duration_cast<std::chrono::milliseconds>(alarm - std::chrono::steady_clock::now()).count();
        if (tickless < 0)
            tickless = 0;
        try {
            zmq::poll(items, 1, tickless);
        } catch (const zmq::error_t& e) {
            break; //  Interrupted
        }
        if (items[0].revents & ZMQ_POLLIN) {
            auto kv = kvmsg::recv(subscriber);
            if (!kv) break;
            if (kv->sequence() > sequence) {
                sequence = kv->sequence();
                kvmap[kv->key()] = *kv;
                std::cout << "I: received update=" << sequence << std::endl;
            }
        }
        if (std::chrono::steady_clock::now() >= alarm) {
            //  Send random update to server
            std::string key = std::to_string(within(10000));
            kvmsg kv(key, 0, (unsigned char *)std::to_string(within(1000000)).c_str());
            kv.send(publisher);
            alarm = std::chrono::steady_clock::now() + std::chrono::seconds(1);
        }
    }
    std::cout << " Interrupted\n" <<  sequence << " messages in\n" << std::endl;
    return 0;
}