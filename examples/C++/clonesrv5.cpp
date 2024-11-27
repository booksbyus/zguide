//  Clone server - Model Five

#include "kvmsg.hpp"

//  Routing information for a key-value snapshot
typedef struct {
    zmqpp::socket_t *socket; //  ROUTER socket to send to
    std::string identity; //  Identity of peer who requested state
    std::string subtree; //  Client subtree specification
} kvroute_t;

typedef struct {
    zmqpp::context_t *ctx;  // Our context
    std::unordered_map<std::string, KVMsg*> kvmap; // Key-value store
    int64_t sequence;  // How many updates we're at
    int port;          // Main port we're working on
    zmqpp::socket_t* snapshot;    // Handle snapshot requests
    zmqpp::socket_t* publisher;   // Publish updates to clients
    zmqpp::socket_t* collector;   // Collect updates from clients
} clonesrv_t;

// loop event handlers
static bool s_snapshots(clonesrv_t *self);
static bool s_collector(clonesrv_t *self);
static bool s_flush_ttl(clonesrv_t *self);

int main(void) {
    zmqpp::loop loop;  // Reactor loop
    clonesrv_t *self = new clonesrv_t();
    self->port = 5556;
    self->ctx = new zmqpp::context_t();

    // set up our clone server sockets
    self->snapshot = new zmqpp::socket_t(*self->ctx, zmqpp::socket_type::router);
    self->snapshot->bind("tcp://*:" + std::to_string(self->port));
    self->publisher = new zmqpp::socket_t(*self->ctx, zmqpp::socket_type::pub);
    self->publisher->bind("tcp://*:" + std::to_string(self->port + 1));
    self->collector = new zmqpp::socket_t(*self->ctx, zmqpp::socket_type::pull);
    self->collector->bind("tcp://*:" + std::to_string(self->port + 2));

    loop.add(*self->snapshot, std::bind(s_snapshots, self));
    loop.add(*self->collector, std::bind(s_collector, self));
    loop.add(std::chrono::milliseconds(1000), 0, std::bind(s_flush_ttl, self));
    s_catch_signals();
    auto end_loop = []() -> bool {
        return s_interrupted == 0;
    };
    loop.add(std::chrono::milliseconds(100), 0, end_loop);

    try {
        loop.start();
    } catch (const std::exception &e) {
        std::cerr << "Exception: " << e.what() << std::endl;
    }
    KVMsg::clear_kvmap(self->kvmap);
    std::cout << "Interrupted\n";
    return 0;
}

//  .split snapshot handler
//  This is the reactor handler for the snapshot socket; it accepts
//  just the ICANHAZ? request and replies with a state snapshot ending
//  with a KTHXBAI message:

static bool s_snapshots(clonesrv_t *self) {
    zmqpp::message frames;
    if (!self->snapshot->receive(frames)) {
        return false;
    }
    std::string identity;
    frames >> identity;
    std::string request;
    frames >> request;
    std::string subtree;
    if (request == "ICANHAZ?") {
        assert(frames.parts() == 3);
        frames >> subtree;
    } else {
        std::cerr << "E: bad request, aborting" << std::endl;
    }

    if (!subtree.empty()) {
        kvroute_t routing = {self->snapshot, identity, subtree};
        for (auto &kv : self->kvmap) {
            if (subtree.size() <= kv.first.size() && kv.first.compare(0, subtree.size(), subtree) == 0) {
                zmqpp::message_t frames;
                frames << identity;
                kv.second->encode_frames(frames);
                routing.socket->send(frames);
            }
        }
        std::cout << "I: sending snapshot=" << self->sequence << std::endl;
        KVMsg *kvmsg = new KVMsg(self->sequence);
        kvmsg->set_key("KTHXBAI");
        kvmsg->set_body(ustring((unsigned char *)subtree.c_str(), subtree.size()));
        // remember to send the identity frame
        zmqpp::message_t frames;
        frames << identity;
        kvmsg->encode_frames(frames);
        self->snapshot->send(frames);
        delete kvmsg;
    }
    return true;
}

//  .split collect updates
//  We store each update with a new sequence number, and if necessary, a
//  time-to-live. We publish updates immediately on our publisher socket:

static bool s_collector(clonesrv_t *self) {
    KVMsg *kvmsg = KVMsg::recv(*self->collector);
    if (!kvmsg) {
        return false;
    }
    kvmsg->set_sequence(++self->sequence);
    kvmsg->send(*self->publisher);
    std::string ttl_second_str = kvmsg->property("ttl");
    if (!ttl_second_str.empty()) {
        int ttl_second = std::atoi(ttl_second_str.c_str());
        auto now = std::chrono::high_resolution_clock::now();
        auto expired_at = std::chrono::duration_cast<std::chrono::milliseconds>(now.time_since_epoch()).count() + ttl_second * 1000;
        kvmsg->set_property("ttl", "%lld", expired_at);
    }
    kvmsg->store(self->kvmap);
    return true;
}

//  .split flush ephemeral values
//  At regular intervals, we flush ephemeral values that have expired. This
//  could be slow on very large data sets:

//  If key-value pair has expired, delete it and publish the
//  fact to listening clients.

static bool s_flush_ttl(clonesrv_t *self) {
    auto now = std::chrono::high_resolution_clock::now();
    for (auto it = self->kvmap.begin(); it != self->kvmap.end();) {
        KVMsg *kvmsg = it->second;
        std::string ttl_str = kvmsg->property("ttl");
        if (!ttl_str.empty()) {
            int64_t ttl = std::atoll(ttl_str.c_str());
            if (ttl < std::chrono::duration_cast<std::chrono::milliseconds>(now.time_since_epoch()).count()) {
                kvmsg->set_sequence(++self->sequence);
                kvmsg->set_body(ustring());
                kvmsg->send(*self->publisher);
                it = self->kvmap.erase(it);
                std::cout << "I: publishing delete=" << self->sequence << std::endl;
            } else {
                ++it;
            }
        } else {
            ++it;
        }
    }
    return true;
}