/*  =====================================================================
 *  flcliapi - Freelance Pattern agent class
 *  ===================================================================== */

#ifndef FLCLIAPI_H
#define FLCLIAPI_H
#pragma once

#include <chrono>
#include <iostream>
#include <memory>
#include <zmqpp/zmqpp.hpp>

//  We design our client API as a class
class Flcliapi {
   public:
    Flcliapi();
    ~Flcliapi();
    void connect(const std::string& endpoint);
    std::unique_ptr<zmqpp::message> request(zmqpp::message& request);

   private:
    //  Our context
    zmqpp::context context_;
    //  Actor object own flcliapi agent
    zmqpp::actor actor_;
    //  This is the thread that handles our real agent task
    bool agent(zmqpp::socket* pipe, zmqpp::context& context);
};

//  .split backend agent
//  Here we see the backend agent. It runs as an attached thread, talking
//  to its parent over a pipe socket. It is a fairly complex piece of work
//  so we'll break it down into pieces. First, the agent manages a set of
//  servers:

//  Simple class for one server we talk to

class Server {
   public:
    Server(const std::string& endpoint);
    ~Server();

    void setPingAt(const std::chrono::time_point<std::chrono::steady_clock>& ping_at) {
        ping_at_ = ping_at;
    }
    void setExpires(const std::chrono::time_point<std::chrono::steady_clock>& expires) {
        expires_ = expires;
    }

    std::chrono::time_point<std::chrono::steady_clock>& getPingAt() { return ping_at_; }

    bool isAlive() { return alive_; }
    void setAlive(bool alive) { alive_ = alive; }
    std::string& getEndpoint() { return endpoint_; }
    std::chrono::time_point<std::chrono::steady_clock>& getExpires() { return expires_; }

   private:
    //  Server identity/endpoint
    std::string endpoint_;
    //  true if known to be alive
    bool alive_;
    //  Next ping at this time
    std::chrono::time_point<std::chrono::steady_clock> ping_at_;
    //  Expires at this time
    std::chrono::time_point<std::chrono::steady_clock> expires_;

   public:
    int ping(zmqpp::socket& socket);
    int tickless(std::chrono::time_point<std::chrono::steady_clock>& tickless_at);
};

//  .split backend agent class
//  We build the agent as a class that's capable of processing messages
//  coming in from its various sockets:

//  Simple class for one background agent

class Agent {
   public:
    Agent(zmqpp::context& context, zmqpp::socket* pipe);
    ~Agent();
    void control_message(std::unique_ptr<zmqpp::message> msg);
    void router_message();
    zmqpp::socket* getPipe() { return pipe_; }
    zmqpp::socket& getRouter() { return router_; }

   private:
    //  Own context
    zmqpp::context& context_;
    //  Socket to talk back to application
    zmqpp::socket* pipe_;
    //  Socket to talk to servers
    zmqpp::socket router_;

   public:
    //  Servers we've connected to
    std::map<std::string, std::shared_ptr<Server>> servers_;
    //  Servers we know are alive
    std::list<std::shared_ptr<Server>> actives_;
    //  Number of requests ever sent
    uint sequence_;
    //  Current request if any
    std::unique_ptr<zmqpp::message> request_;
    //  Current reply if any
    std::unique_ptr<zmqpp::message> reply_;
    //  Timeout for request/reply
    std::chrono::time_point<std::chrono::steady_clock> expires_;
};
#endif