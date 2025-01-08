//  clone class - Clone client API stack (multithreaded)

#include <string>
#include <zmqpp/zmqpp.hpp>

#include "kvmsg.hpp"
//  If no server replies within this time, abandon request
#define GLOBAL_TIMEOUT 4000 // msecs

class clone_t {
public:
    clone_t();
    ~clone_t();
    void connect(std::string endpoint, std::string service);
    void subtree(std::string subtree);
    void set(const std::string &key, const std::string &value, int ttl = 0);
    std::optional<std::string> get(const std::string &key);
private:
    bool clone_agent(zmqpp::socket_t *pipe, zmqpp::context_t *ctx);

    zmqpp::context_t *ctx; //  Our context wrapper
    zmqpp::actor *actor; //  Pipe through to clone agent
};

class server_t {
public:
    server_t(zmqpp::context_t *ctx, std::string address, int port, std::string subtree);
    ~server_t();
public:
    std::string address;   //  Server address
    int port;              //  Server port
    zmqpp::socket_t *snapshot; //  DEALER socket for snapshot
    zmqpp::socket_t *subscriber; //  SUB socket for updates
    std::chrono::time_point<std::chrono::steady_clock> expiry;       //  When server expires
    uint requests;         //  How many snapshot requests made?
};

//  .split backend agent class
//  Here is the implementation of the backend agent itself:

//  Number of servers to which we will talk to
#define SERVER_MAX 2

// Server considered dead if silent for this long
#define SERVER_TTL 5000 // msecs

// States we can be in
#define STATE_INITIAL 0 // Before asking server for state
#define STATE_SYNCING 1 // Getting state from server
#define STATE_ACTIVE 2  // Getting new updates from server

class agent_t {
    friend class clone_t;
public:
    agent_t(zmqpp::context_t *ctx, zmqpp::socket_t *pipe);
    ~agent_t();

private:
    int agent_control_message();

    zmqpp::context_t *ctx; // Context wrapper
    zmqpp::socket_t *pipe; // Pipe back to application
    std::unordered_map<std::string, KVMsg*> kvmap; // Actual key/value table
    std::string subtree; // Subtree specification, if any
    server_t *servers[SERVER_MAX];
    uint nbr_servers; // 0 to SERVER_MAX
    uint state; // Current state
    uint cur_server; // If active, server 0 or 1
    int64_t sequence; // Last kvmsg processed
    zmqpp::socket_t *publisher; // Outgoing updates
};


// Implementations

//  .split constructor and destructor
//  Here are the constructor and destructor for the clone class. Note that
//  we create a context specifically for the pipe that connects our
//  frontend to the backend agent:

clone_t::clone_t() {
    ctx = new zmqpp::context_t();
    actor = new zmqpp::actor(std::bind(&clone_t::clone_agent, this, std::placeholders::_1, ctx));
}

clone_t::~clone_t() {
    delete actor;
    delete ctx;
}

//  .split connect method
//  Connect to a new server endpoint. We can connect to at most two
//  servers. Sends [CONNECT][endpoint][service] to the agent:

void clone_t::connect(std::string endpoint, std::string service) {
    zmqpp::message msg;
    msg << "CONNECT" << endpoint << service;
    actor->pipe()->send(msg);
    std::this_thread::sleep_for(std::chrono::milliseconds(100)); // Allow connection to come up
}

//  .split subtree method
//  Specify subtree for snapshot and updates, which we must do before
//  connecting to a server as the subtree specification is sent as the
//  first command to the server. Sends a [SUBTREE][subtree] command to
//  the agent:

void clone_t::subtree(std::string subtree) {
    zmqpp::message msg;
    msg << "SUBTREE" << subtree;
    actor->pipe()->send(msg);
}

//  .split set method
//  Set a new value in the shared hashmap. Sends a [SET][key][value][ttl]
//  command through to the agent which does the actual work:

void clone_t::set(const std::string &key, const std::string &value, int ttl) {
    zmqpp::message msg;
    msg << "SET" << key << value << ttl;
    actor->pipe()->send(msg);
}

//  .split get method
//  Look up value in distributed hash table. Sends [GET][key] to the agent and
//  waits for a value response. If there is no value available, will eventually
//  return NULL:

std::optional<std::string> clone_t::get(const std::string &key) {
    zmqpp::message msg;
    msg << "GET" << key;
    actor->pipe()->send(msg);
    zmqpp::message reply;
    actor->pipe()->receive(reply);
    if (reply.parts() == 0) {
        return std::nullopt;
    }
    return reply.get(0);
}

//  .split working with servers
//  The backend agent manages a set of servers, which we implement using
//  our simple class model:
server_t::server_t(zmqpp::context_t *ctx, std::string address, int port, std::string subtree) {
    this->address = address;
    this->port = port;

    snapshot = new zmqpp::socket_t(*ctx, zmqpp::socket_type::dealer);
    snapshot->connect(address + ":" + std::to_string(port));
    std::cout << "server_t: connected to snapshot at " << address << ":" << port << std::endl;

    subscriber = new zmqpp::socket_t(*ctx, zmqpp::socket_type::sub);
    subscriber->connect(address + ":" + std::to_string(port + 1));
    subscriber->set(zmqpp::socket_option::subscribe, subtree);
    subscriber->set(zmqpp::socket_option::subscribe, "HUGZ");
    std::cout << "server_t: connected to subscribe at " << address << ":" << port+1 << std::endl;
}

server_t::~server_t() {
    delete snapshot;
    delete subscriber;
}

//  .split backend agent class
//  Here is the implementation of the backend agent itself:

agent_t::agent_t(zmqpp::context_t *ctx, zmqpp::socket_t *pipe) {
    this->ctx = ctx;
    this->pipe = pipe;
    this->publisher = new zmqpp::socket_t(*ctx, zmqpp::socket_type::pub);
    this->state = STATE_INITIAL;
    this->cur_server = 0;
    this->sequence = 0;
    this->nbr_servers = 0;
}

agent_t::~agent_t() {
    delete publisher;
    for (uint server_nbr = 0; server_nbr < nbr_servers; server_nbr++) {
        delete servers[server_nbr];
    }
}

//  .split handling a control message
//  Here we handle the different control messages from the frontend;
//  SUBTREE, CONNECT, SET, and GET:

int agent_t::agent_control_message() {
    zmqpp::message msg;
    pipe->receive(msg);
    std::string command;
    msg >> command;
    std::cout << "I: agent_control_message: " << command << std::endl;
    if (command == "") {
        return -1; // Interrupted
    }
    if (command == "SUBTREE") {
        msg >> subtree;
    } else if (command == "CONNECT") {
        std::string address, service;
        msg >> address >> service;
        if (nbr_servers < SERVER_MAX) {
            servers[nbr_servers++] = new server_t(ctx, address, std::stoi(service), subtree);
            // We broadcast updates to all known servers
            publisher->connect(address + ":" + std::to_string(std::stoi(service) + 2));
            std::cout << "I: publish to server at " << address << ":" << service << std::endl;
        } else {
            std::cerr << "Too many servers" << std::endl;
        }
    } else if (command == "SET") {
        std::string key, value;
        int ttl;
        msg >> key >> value >> ttl;
        KVMsg *kvmsg = new KVMsg(0);
        kvmsg->set_key(key);
        kvmsg->set_uuid();
        kvmsg->fmt_body("%s", value.c_str());
        kvmsg->set_property("ttl", "%d", ttl);
        kvmsg->store(kvmap);
        kvmsg->send(*publisher);
    } else if (command == "GET") {
        std::string key;
        msg >> key;
        auto it = kvmap.find(key);
        if (it != kvmap.end()) {
            KVMsg *kvmsg = it->second;
            zmqpp::message reply;
            reply << std::string((char *)kvmsg->body().data(), kvmsg->size());
            pipe->send(reply);
        } else {
            pipe->send("");
        }
    }
    return 0;
}

//  .split backend agent
//  The asynchronous agent manages a server pool and handles the
//  request-reply dialog when the application asks for it:

bool clone_t::clone_agent(zmqpp::socket_t *pipe, zmqpp::context_t *ctx) {
    pipe->send(zmqpp::signal::ok); // Signal successful startup
    std::cout << "I: clone agent started" << std::endl;
    agent_t *agent = new agent_t(ctx, pipe);

    while (true) {
        zmqpp::poller poller;
        poller.add(*agent->pipe);

        int tickless = 0;

        server_t *server = agent->servers[agent->cur_server];
        switch (agent->state) {
            case STATE_INITIAL:
                //  In this state we ask the server for a snapshot,
                //  if we have a server to talk to...
                if (agent->nbr_servers > 0) {
                    std::cout << "Waiting for server at " << server->address << ":" << server->port << std::endl;
                    if (server->requests < 2) {
                        zmqpp::message msg;
                        msg << "ICANHAZ?" << agent->subtree;
                        server->snapshot->send(msg);
                        server->requests++;
                    }
                    server->expiry = std::chrono::steady_clock::now() + std::chrono::milliseconds(SERVER_TTL);
                    // std::cout << "server->expiry=" << server->expiry.time_since_epoch().count() << std::endl;
                    agent->state = STATE_SYNCING;
                    poller.add(*server->snapshot);
                }
                break;
            case STATE_SYNCING:
                //  In this state we read from snapshot and we expect
                //  the server to respond, else we fail over.
                poller.add(*server->snapshot);
                break;
            case STATE_ACTIVE:
                //  In this state we read from subscriber and we expect
                //  the server to give HUGZ, else we fail over.
                poller.add(*server->subscriber);
                break;
        }

        if (server) {
            tickless = std::chrono::duration_cast<std::chrono::milliseconds>(server->expiry - std::chrono::steady_clock::now()).count();
            if (tickless < 0) 
                tickless = 0;
        }
        //  .split client poll loop
        //  We're ready to process incoming messages; if nothing at all
        //  comes from our server within the timeout, that means the
        //  server is dead:

        if (poller.poll(tickless)) {
            //  poll return true if we have event
            if (poller.has_input(*agent->pipe)) {
                if (agent->agent_control_message() != 0) {
                    break;
                }
            } else if (agent->state == STATE_SYNCING && poller.has_input(*server->snapshot)) {
                server->expiry = std::chrono::steady_clock::now() + std::chrono::milliseconds(SERVER_TTL);
                KVMsg *kvmsg = KVMsg::recv(*server->snapshot);
                if (kvmsg) {
                    if (kvmsg->key() == "KTHXBAI") {
                        agent->sequence = kvmsg->sequence();
                        std::cout << "I: recieved from " << server->address << ":" << server->port << " snapshot=" << agent->sequence << std::endl;
                        delete kvmsg;
                        agent->state = STATE_ACTIVE;
                    } else {
                        kvmsg->store(agent->kvmap);
                    }
                } else {
                    break;
                }
            } else if (agent->state == STATE_ACTIVE && poller.has_input(*server->subscriber)) {
                server->expiry = std::chrono::steady_clock::now() + std::chrono::milliseconds(SERVER_TTL);
                KVMsg *kvmsg = KVMsg::recv(*server->subscriber);
                if (kvmsg) {
                    //  Discard out-of-sequence updates, incl. HUGZ
                    if (kvmsg->sequence() > agent->sequence) {
                        agent->sequence = kvmsg->sequence();
                        kvmsg->store(agent->kvmap);
                        std::cout << "I: recieved from " << server->address << ":" << server->port << " update=" << kvmsg->sequence() << std::endl;
                    } else {
                        delete kvmsg;
                    }
                } else {
                    break;
                }
            }
        } else {
            // poll return false if timeout
            // Server has died, failover to next
            std::cout << "I: server at " << server->address << ":" << server->port << " didn't give HUGZ" << std::endl;
            agent->cur_server = (agent->cur_server + 1) % agent->nbr_servers;
            agent->state = STATE_INITIAL;
        }
    }
    delete agent;
    return true; // will send signal::ok to signal successful shutdown
}