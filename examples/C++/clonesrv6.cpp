#include "bstar.hpp"
#include "kvmsg.hpp"

typedef struct {
    zmqpp::context_t *ctx;  // Our context
    std::unordered_map<std::string, KVMsg*>* kvmap; // Key-value store
    bstar_t *bstar;     // Binary Star reactor core
    int64_t sequence;  // How many updates we're at
    int peer;           // Main port of our peer
    int port;          // Main port we're working on
    zmqpp::socket_t* publisher;   // Publish updates and hugz
    zmqpp::socket_t* collector;   // Collect updates from clients
    zmqpp::socket_t* subscriber;  // Receive updates from peer
    std::list<KVMsg*> pending;  // Pending updates
    bool primary;       // true if we're primary
    bool active;        // true if we're active
    bool passive;       // true if we're passive
} clonesrv_t;


static void s_snapshot(zmqpp::loop *loop, zmqpp::socket_t *socket, void *arg);
static bool s_collector(clonesrv_t *self);
static bool s_flush_ttl(clonesrv_t *self);
static bool s_send_hugz(clonesrv_t *self);
static bool s_subscriber(clonesrv_t *self);
static void s_new_active(zmqpp::loop *loop, zmqpp::socket_t *socket, void *arg);
static void s_new_passive(zmqpp::loop *loop, zmqpp::socket_t *socket, void *arg);

//  .split main task setup
//  The main task parses the command line to decide whether to start
//  as a primary or backup server. We're using the Binary Star pattern
//  for reliability. This interconnects the two servers so they can
//  agree on which one is primary and which one is backup. To allow the
//  two servers to run on the same box, we use different ports for 
//  primary and backup. Ports 5003/5004 are used to interconnect the 
//  servers. Ports 5556/5566 are used to receive voting events (snapshot 
//  requests in the clone pattern). Ports 5557/5567 are used by the 
//  publisher, and ports 5558/5568 are used by the collector:
int main(int argc, char *argv[]) {
    clonesrv_t *self = new clonesrv_t();
    if (argc == 2 && strcmp(argv[1], "-p") == 0) {
        std::cout << "I: Primary active, waiting for backup (passive)" << std::endl;
        self->bstar = new bstar_t(true, "tcp://*:5003", "tcp://localhost:5004");
        self->bstar->register_voter("tcp://*:5556", zmqpp::socket_type::router, s_snapshot, self);
        self->port = 5556;
        self->peer = 5566;
        self->primary = true;
    } else if (argc == 2 && strcmp(argv[1], "-b") == 0) {
        std::cout << "I: Backup passive, waiting for primary (active)" << std::endl;
        self->bstar = new bstar_t(false, "tcp://*:5004", "tcp://localhost:5003");
        self->bstar->register_voter("tcp://*:5566", zmqpp::socket_type::router, s_snapshot, self);
        self->port = 5566;
        self->peer = 5556;
        self->primary = false;
    } else {
        std::cout << "Usage: clonesrv6 { -p | -b }" << std::endl;
        delete self;
        return 0;
    }

    if (self->primary) {
        self->kvmap = new std::unordered_map<std::string, KVMsg*>();
    }
    self->ctx = new zmqpp::context_t();

    // set up our clone server sockets
    self->publisher = new zmqpp::socket_t(*self->ctx, zmqpp::socket_type::pub);
    self->publisher->bind("tcp://*:" + std::to_string(self->port + 1));
    self->collector = new zmqpp::socket_t(*self->ctx, zmqpp::socket_type::sub);
    self->collector->subscribe("");
    self->collector->bind("tcp://*:" + std::to_string(self->port + 2)); // allow multiple clients publish updates

    // set up our own clone client interface to peer
    self->subscriber = new zmqpp::socket_t(*self->ctx, zmqpp::socket_type::sub);
    self->subscriber->subscribe("");
    self->subscriber->connect("tcp://localhost:" + std::to_string(self->peer + 1));

    //  .split main task body
    //  After we've setup our sockets, we register our binary star
    //  event handlers, and then start the bstar reactor. This finishes
    //  when the user presses Ctrl-C or when the process receives a SIGINT
    //  interrupt:
    
    // Register state change handlers
    self->bstar->set_new_active(s_new_active, self);
    self->bstar->set_new_passive(s_new_passive, self);

    // Register our other handlers with the bstar reactor
    self->bstar->get_loop()->add(*self->collector, std::bind(s_collector, self));
    self->bstar->get_loop()->add(std::chrono::milliseconds(1000), 0, std::bind(s_flush_ttl, self));
    self->bstar->get_loop()->add(std::chrono::milliseconds(1000), 0, std::bind(s_send_hugz, self));
    
    s_catch_signals();
    auto end_loop = []() -> bool {
        return s_interrupted == 0;
    };
    self->bstar->get_loop()->add(std::chrono::milliseconds(100), 0, end_loop);

    //  Start the bstar reactor
    try {
        self->bstar->start();
    } catch (const std::exception &e) {
        std::cerr << "Exception: " << e.what() << std::endl;
    }

    // Clean up
    KVMsg::clear_kvmap(*self->kvmap);
    delete self;

    return 0;
}

//  Routing information for a key-value snapshot
typedef struct {
    zmqpp::socket_t *socket; //  ROUTER socket to send to
    std::string identity; //  Identity of peer who requested state
    std::string subtree; //  Client subtree specification
} kvroute_t;


static void s_snapshot(zmqpp::loop *loop, zmqpp::socket_t *socket, void *arg) {
    clonesrv_t *self = (clonesrv_t *) arg;

    zmqpp::message frames;
    if (!socket->receive(frames)) {
        return;
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

    // if (!subtree.empty()) {
    kvroute_t routing = {socket, identity, subtree};
    for (auto &kv : *self->kvmap) {
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
    zmqpp::message_t response_frames;
    response_frames << identity;
    kvmsg->encode_frames(response_frames);
    socket->send(response_frames);
    delete kvmsg;
    // }
}

//  .split collect updates
//  The collector is more complex than in the clonesrv5 example because the 
//  way it processes updates depends on whether we're active or passive. 
//  The active applies them immediately to its kvmap, whereas the passive 
//  queues them as pending:

//  If message was already on pending list, remove it and return true,
//  else return false.
static bool s_was_pending(clonesrv_t *self, KVMsg *kvmsg) {
    for (auto it = self->pending.begin(); it != self->pending.end(); ++it) {
        if ((*it)->uuid() == kvmsg->uuid()) {
            self->pending.erase(it);
            return true;
        }
    }
    return false;
}

static bool s_collector(clonesrv_t *self) {
    KVMsg *kvmsg = KVMsg::recv(*self->collector);
    if (!kvmsg) {
        return false;
    }
    if (self->active) {
        kvmsg->set_sequence(++self->sequence);
        kvmsg->send(*self->publisher);
        std::string ttl_second_str = kvmsg->property("ttl");
        if (!ttl_second_str.empty()) {
            int ttl_second = std::atoi(ttl_second_str.c_str());
            auto now = std::chrono::high_resolution_clock::now();
            auto expired_at = std::chrono::duration_cast<std::chrono::milliseconds>(now.time_since_epoch()).count() + ttl_second * 1000;
            kvmsg->set_property("ttl", "%lld", expired_at);
        }
        kvmsg->store(*self->kvmap);
        std::cout << "I: publishing update=" << self->sequence << std::endl;
    } else {
        //  If we already got message from active, drop it, else
        //  hold on pending list
        if (s_was_pending(self, kvmsg)) {
            // std::cout << "DEBUG: already got from active server, drop it" << std::endl;
            delete kvmsg;
        } else {
            // std::cout << "DEBUG: hold client request on pending list" << std::endl;
            self->pending.push_back(kvmsg);
        }
    }
    return true;
}

//  We purge ephemeral values using exactly the same code as in
//  the previous clonesrv5 example.
//  .skip
//  If key-value pair has expired, delete it and publish the
//  fact to listening clients.
static bool s_flush_ttl(clonesrv_t *self) {
    auto now = std::chrono::high_resolution_clock::now();
    if (!self->kvmap) {
        return true;
    }
    for (auto it = self->kvmap->begin(); it != self->kvmap->end();) {
        KVMsg *kvmsg = it->second;
        std::string ttl_str = kvmsg->property("ttl");
        if (!ttl_str.empty()) {
            int64_t ttl = std::atoll(ttl_str.c_str());
            if (ttl < std::chrono::duration_cast<std::chrono::milliseconds>(now.time_since_epoch()).count()) {
                kvmsg->set_sequence(++self->sequence);
                kvmsg->set_body(ustring());
                kvmsg->send(*self->publisher);
                it = self->kvmap->erase(it);
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

//  .until

//  .split heartbeating
//  We send a HUGZ message once a second to all subscribers so that they
//  can detect if our server dies. They'll then switch over to the backup
//  server, which will become active:
static bool s_send_hugz(clonesrv_t *self) {
    KVMsg *kvmsg = new KVMsg(self->sequence);
    kvmsg->set_key("HUGZ");
    kvmsg->set_body(ustring());
    kvmsg->send(*self->publisher);
    delete kvmsg;
    return true;
}

//  .split handling state changes
//  When we switch from passive to active, we apply our pending list so that
//  our kvmap is up-to-date. When we switch to passive, we wipe our kvmap
//  and grab a new snapshot from the active server:

static void s_new_active(zmqpp::loop *loop, zmqpp::socket_t *socket, void *arg) {
    clonesrv_t *self = (clonesrv_t *)arg;
    self->active = true;
    self->passive = false;

    // Stop subscribing to updates
    self->bstar->get_loop()->remove(*self->subscriber);

    // Apply pending list to own hash table
    while (!self->pending.empty()) {
        KVMsg *kvmsg = self->pending.front();
        kvmsg->set_sequence(++self->sequence);
        kvmsg->send(*self->publisher);
        kvmsg->store(*self->kvmap);
        std::cout << "I: publishing pending=" << self->sequence << std::endl;
        self->pending.pop_front();
    }
}

static void s_new_passive(zmqpp::loop *loop, zmqpp::socket_t *socket, void *arg) {
    clonesrv_t *self = (clonesrv_t *)arg;
    if (self->kvmap) {
        KVMsg::clear_kvmap(*self->kvmap);
        self->kvmap = nullptr;
    }
    self->active = false;
    self->passive = true;

    // Start subscribing to updates
    self->bstar->get_loop()->add(*self->subscriber, std::bind(s_subscriber, self));
}


//  .split subscriber handler
//  When we get an update, we create a new kvmap if necessary, and then
//  add our update to our kvmap. We're always passive in this case:
static bool s_subscriber(clonesrv_t *self) {
    //  Get state snapshot if necessary
    if (self->kvmap == nullptr) {
        self->kvmap = new std::unordered_map<std::string, KVMsg*>();
        zmqpp::socket_t snapshot(*self->ctx, zmqpp::socket_type::dealer);
        snapshot.connect("tcp://localhost:" + std::to_string(self->peer));
        std::cout << "I: asking for snapshot from: tcp://localhost:" << self->peer << std::endl;
        zmqpp::message_t frames;
        frames << "ICANHAZ?" << ""; // blank subtree
        snapshot.send(frames);
        while (true) {
            KVMsg *kvmsg = KVMsg::recv(snapshot);
            if (!kvmsg) {
                break;
            }
            if (kvmsg->key() == "KTHXBAI") {
                self->sequence = kvmsg->sequence();
                delete kvmsg;
                break;
            }
            kvmsg->store(*self->kvmap);
        }
        std::cout << "I: received snapshot=" << self->sequence << std::endl;
    }
    KVMsg *kvmsg = KVMsg::recv(*self->subscriber);
    if (!kvmsg) {
        return false;
    }
    if (kvmsg->key() != "HUGZ") {
        if (!s_was_pending(self, kvmsg)) {
            //  If active update came before client update, flip it
            //  around, store active update (with sequence) on pending
            //  list and use to clear client update when it comes later
            self->pending.push_back(kvmsg);
        }
        // If update is more recent than our kvmap, apply it
        if (kvmsg->sequence() > self->sequence) {
            self->sequence = kvmsg->sequence();
            kvmsg->store(*self->kvmap);
            std::cout << "I: received update=" << self->sequence << std::endl;
        } else {
            delete kvmsg;
        }
    } else {
        delete kvmsg;
    }
    return true;
}