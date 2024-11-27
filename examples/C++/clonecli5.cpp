// Clone client - Model Five

#include "kvmsg.hpp"

// #define SUBTREE "/client/"
std::string SUBTREE = "/client/";

int main(int argc, char *argv[]) {
    if (argc > 1) {
        SUBTREE = argv[1];
    }
    zmqpp::context_t context;
    zmqpp::socket_t snapshot(context, zmqpp::socket_type::dealer);
    snapshot.connect("tcp://localhost:5556");
    zmqpp::socket_t subscriber(context, zmqpp::socket_type::subscribe);
    subscriber.set(zmqpp::socket_option::subscribe, SUBTREE);
    subscriber.connect("tcp://localhost:5557");
    zmqpp::socket_t publisher(context, zmqpp::socket_type::push);
    publisher.connect("tcp://localhost:5558");

    std::unordered_map<std::string, KVMsg*> kvmap;

    //  Get state snapshot
    int64_t sequence = 0;
    zmqpp::message msg;
    msg << "ICANHAZ?" << SUBTREE;
    snapshot.send(msg);
    while (true) {
        KVMsg* kv = KVMsg::recv(snapshot);
        if (!kv) break;
        std::cout << "I: received snapshot kvmsg:" << kv->to_string() << std::endl;
        if (kv->key() == "KTHXBAI") {
            sequence = kv->sequence();
            std::cout << "I: received snapshot=" << sequence << std::endl;
            delete kv;
            break;
        }
        kv->store(kvmap);
    }
    s_catch_signals();
    std::chrono::time_point<std::chrono::steady_clock> alarm = std::chrono::steady_clock::now() + std::chrono::seconds(1);
    while (!s_interrupted) {
        zmqpp::poller_t poller;
        poller.add(subscriber, zmqpp::poller_t::poll_in);
        int tickless = std::chrono::duration_cast<std::chrono::milliseconds>(alarm - std::chrono::steady_clock::now()).count();
        if (tickless < 0) tickless = 0;
        poller.poll(tickless);
        if (poller.has_input(subscriber)) {
            KVMsg* kv = KVMsg::recv(subscriber);
            if (!kv) break;
            if (kv->sequence() > sequence) {
                sequence = kv->sequence();
                kv->store(kvmap);
                std::cout << "I: received update=" << sequence << std::endl;
            }
        }
        if (std::chrono::steady_clock::now() >= alarm) {
            KVMsg kv(0);
            kv.fmt_key("%s%d", SUBTREE.c_str(), within(10000));
            kv.fmt_body("%d", within(1000000));
            kv.set_property("ttl", "%d", within(30)); // within 30 seconds
            std::cout << "I: publish kvmsg:" << kv.to_string() << std::endl;
            kv.send(publisher);
            alarm = std::chrono::steady_clock::now() + std::chrono::seconds(1);
        }
    }
    std::cout << "Interrupted\n" << sequence << " messages handled\n";
    KVMsg::clear_kvmap(kvmap);
    return 0;
}