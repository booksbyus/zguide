//  flcliapi class - Freelance Pattern agent class
//  Implements the Freelance Protocol at http://rfc.zeromq.org/spec:10

#include "flcliapi.hpp"

//  If no server replies within this time, abandon request
const int GLOBAL_TIMEOUT = 3000;  //  msecs
//  PING interval for servers we think are alive
const int PING_INTERVAL = 500;  //  msecs
//  Server considered dead if silent for this long
const int SERVER_TTL = 1000;  //  msecs

//  This API works in two halves, a common pattern for APIs that need to
//  run in the background. One half is an frontend object our application
//  creates and works with; the other half is a backend "agent" that runs
//  in a background thread. The frontend talks to the backend over an
//  inproc pipe socket created by actor object:

//  Constructor
Flcliapi::Flcliapi()
    : actor_(std::bind(&Flcliapi::agent, this, std::placeholders::_1, std::ref(context_))) {}

Flcliapi::~Flcliapi() {}

//  connect interface
//  To implement the connect method, the frontend object sends a multipart
//  message to the backend agent. The first part is a string "CONNECT", and
//  the second part is the endpoint. It waits 100msec for the connection to
//  come up, which isn't pretty, but saves us from sending all requests to a
//  single server, at startup time:
void Flcliapi::connect(const std::string& endpoint) {
    zmqpp::message msg;
    msg.push_back("CONNECT");
    msg.push_back(endpoint);
    actor_.pipe()->send(msg);
    std::this_thread::sleep_for(std::chrono::milliseconds(100));  //  Allow connection to come up
}

//  request interface
//  To implement the request method, the frontend object sends a message
//  to the backend, specifying a command "REQUEST" and the request message:
std::unique_ptr<zmqpp::message> Flcliapi::request(zmqpp::message& request) {
    assert(request.parts() > 0);
    request.push_front("REQUEST");
    actor_.pipe()->send(request);
    std::unique_ptr<zmqpp::message> reply = std::make_unique<zmqpp::message>();
    actor_.pipe()->receive(*reply);
    if (0 != reply->parts()) {
        if (reply->get(0) == "FAILED") reply.release();
    } else {
        reply.release();
    }
    return reply;
}

Server::Server(const std::string& endpoint) {
    endpoint_ = endpoint;
    alive_ = false;
    ping_at_ = std::chrono::steady_clock::now() + std::chrono::milliseconds(PING_INTERVAL);
    expires_ = std::chrono::steady_clock::now() + std::chrono::milliseconds(SERVER_TTL);
}

Server::~Server() {}

int Server::ping(zmqpp::socket& socket) {
    if (std::chrono::steady_clock::now() >= ping_at_) {
        zmqpp::message ping;
        ping.push_back(endpoint_);
        ping.push_back("PING");
        socket.send(ping);
        ping_at_ = std::chrono::steady_clock::now() + std::chrono::milliseconds(PING_INTERVAL);
    }
    return 0;
}

int Server::tickless(std::chrono::time_point<std::chrono::steady_clock>& tickless_at) {
    if (tickless_at > ping_at_) tickless_at = ping_at_;
    return 0;
}

Agent::Agent(zmqpp::context& context, zmqpp::socket* pipe)
    : context_(context), pipe_(pipe), router_(context, zmqpp::socket_type::router) {
    router_.set(zmqpp::socket_option::linger, GLOBAL_TIMEOUT);
    sequence_ = 0;
}
Agent::~Agent() {}

//  control messages
//  This method processes one message from our frontend class
//  (it's going to be CONNECT or REQUEST):
void Agent::control_message(std::unique_ptr<zmqpp::message> msg) {
    std::string command = msg->get(0);
    msg->pop_front();

    if (command == "CONNECT") {
        std::string endpoint = msg->get(0);
        msg->pop_front();
        std::cout << "I: connecting to " << endpoint << "..." << std::endl;
        try {
            router_.connect(endpoint);
        } catch (zmqpp::zmq_internal_exception& e) {
            std::cerr << "failed to bind to endpoint " << endpoint << ": " << e.what() << std::endl;
            return;
        }
        std::shared_ptr<Server> server = std::make_shared<Server>(endpoint);
        servers_.insert(std::pair<std::string, std::shared_ptr<Server>>(endpoint, server));
        // actives_.push_back(server);
        server->setPingAt(std::chrono::steady_clock::now() +
                          std::chrono::milliseconds(PING_INTERVAL));
        server->setExpires(std::chrono::steady_clock::now() +
                           std::chrono::milliseconds(SERVER_TTL));
    } else if (command == "REQUEST") {
        assert(!request_);  //  Strict request-reply cycle
        //  Prefix request with sequence number and empty envelope
        msg->push_front(++sequence_);
        //  Take ownership of request message
        request_ = std::move(msg);
        //  Request expires after global timeout
        expires_ = std::chrono::steady_clock::now() + std::chrono::milliseconds(GLOBAL_TIMEOUT);
    }
}

//  .split router messages
//  This method processes one message from a connected
//  server:
void Agent::router_message() {
    zmqpp::message reply;
    router_.receive(reply);
    //  Frame 0 is server that replied
    std::string endpoint = reply.get(0);
    reply.pop_front();
    assert(servers_.count(endpoint));
    std::shared_ptr<Server> server = servers_.at(endpoint);
    if (!server->isAlive()) {
        actives_.push_back(server);
        server->setAlive(true);
    }
    server->setPingAt(std::chrono::steady_clock::now() + std::chrono::milliseconds(PING_INTERVAL));
    server->setExpires(std::chrono::steady_clock::now() + std::chrono::milliseconds(SERVER_TTL));

    // Frame 1 may be sequence number for reply
    uint sequence;
    reply.get(sequence, 0);
    reply.pop_front();
    if (request_) {
        if (sequence == sequence_) {
            request_.release();
            reply.push_front("OK");
            pipe_->send(reply);
        }
    }
}

//  .split backend agent implementation
//  Finally, here's the agent task itself, which polls its two sockets
//  and processes incoming messages:
bool Flcliapi::agent(zmqpp::socket* pipe, zmqpp::context& context) {
    Agent self(context, pipe);

    zmqpp::poller poller;
    poller.add(*self.getPipe());
    poller.add(self.getRouter());
    pipe->send(zmqpp::signal::ok);  // signal we successfully started
    while (true) {
        //  Calculate tickless timer, up to 1 hour
        std::chrono::time_point<std::chrono::steady_clock> tickless =
            std::chrono::steady_clock::now() + std::chrono::hours(1);
        if (self.request_ && tickless > self.expires_) tickless = self.expires_;
        for (auto& kv : self.servers_) {
            kv.second->tickless(tickless);
        }
        if (poller.poll(std::chrono::duration_cast<std::chrono::milliseconds>(
                            tickless - std::chrono::steady_clock::now())
                            .count())) {
            if (poller.has_input(*self.getPipe())) {
                std::unique_ptr<zmqpp::message> msg = std::make_unique<zmqpp::message>();
                pipe->receive(*msg);
                if (msg->is_signal()) {
                    zmqpp::signal sig;
                    msg->get(sig, 0);
                    if (sig == zmqpp::signal::stop) break;  // actor receive stop signal, exit

                } else
                    self.control_message(std::move(msg));
            }
            if (poller.has_input(self.getRouter())) self.router_message();
        }

        //  If we're processing a request, dispatch to next server
        if (self.request_) {
            if (std::chrono::steady_clock::now() >= self.expires_) {
                //  Request expired, kill it
                self.request_.release();
                self.getPipe()->send("FAILED");
            } else {
                //  Find server to talk to, remove any expired ones
                while (self.actives_.size() > 0) {
                    auto& server = self.actives_.front();
                    if (std::chrono::steady_clock::now() >= server->getExpires()) {
                        server->setAlive(false);
                        self.actives_.pop_front();
                    } else {
                        zmqpp::message request;
                        request.copy(*self.request_);
                        request.push_front(server->getEndpoint());
                        self.getRouter().send(request);
                        break;
                    }
                }
            }
        }

        for (auto& kv : self.servers_) {
            kv.second->ping(self.getRouter());
        }
    }
    return true;  // will send signal::ok to signal successful shutdown
}