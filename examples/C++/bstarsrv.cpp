//  Binary Star server proof-of-concept implementation. This server does no
//  real work; it just demonstrates the Binary Star failover model.

#include "zmsg.hpp"

#define ZMQ_POLL_MSEC 1      //  One second

//  States we can be in at any point in time
typedef enum {
    STATE_NOTSET = 0,           //  Before we start, or if the state is invalid
    STATE_PRIMARY = 1,          //  Primary, waiting for peer to connect
    STATE_BACKUP = 2,           //  Backup, waiting for peer to connect
    STATE_ACTIVE = 3,           //  Active - accepting connections
    STATE_PASSIVE = 4           //  Passive - not accepting connections
} state_t;

//  Events, which start with the states our peer can be in
typedef enum {
    EVENT_NOTSET = 0,           //  Before we start, or if the event is invalid
    PEER_PRIMARY = 1,           //  HA peer is pending primary
    PEER_BACKUP = 2,            //  HA peer is pending backup
    PEER_ACTIVE = 3,            //  HA peer is active
    PEER_PASSIVE = 4,           //  HA peer is passive
    CLIENT_REQUEST = 5          //  Client makes request
} event_t;


//  We send state information this often
//  If peer doesn't respond in two heartbeats, it is 'dead'
#define HEARTBEAT 1000          //  In msecs

//  .split Binary Star state machine
//  The heart of the Binary Star design is its finite-state machine (FSM).
//  The FSM runs one event at a time. We apply an event to the current state,
//  which checks if the event is accepted, and if so, sets a new state:

//  Our finite state machine
class bstar {
public:
    bstar() : m_state(STATE_NOTSET), m_event(EVENT_NOTSET), m_peer_expiry(0) {}

    bool state_machine(event_t event) {
        m_event = event;

        bool exception = false;

        //  These are the PRIMARY and BACKUP states; we're waiting to become
        //  ACTIVE or PASSIVE depending on events we get from our peer:
        if (m_state == STATE_PRIMARY) {
            if (m_event == PEER_BACKUP) {
                std::cout << "I: connected to backup (passive), ready active" << std::endl;
                m_state = STATE_ACTIVE;
            } else if (m_event == PEER_ACTIVE) {
                std::cout << "I: connected to backup (active), ready passive" << std::endl;
                m_state = STATE_PASSIVE;
            }
            //  Accept client connections
        } else if (m_state == STATE_BACKUP) {
            if (m_event == PEER_ACTIVE) {
                std::cout << "I: connected to primary (active), ready passive" << std::endl;
                m_state = STATE_PASSIVE;
            } else if (m_event == CLIENT_REQUEST) {
                //  Reject client connections when acting as backup
                exception = true;
            }
        //  .split active and passive states
        //  These are the ACTIVE and PASSIVE states:
        } else if (m_state == STATE_ACTIVE) {
            if (m_event == PEER_ACTIVE) {
                std::cout << "E: fatal error - dual actives, aborting" << std::endl;
                exception = true;
            }
        //  Server is passive
        //  CLIENT_REQUEST events can trigger failover if peer looks dead
        } else if (m_state == STATE_PASSIVE) {
            if (m_event == PEER_PRIMARY) {
                //  Peer is restarting - become active, peer will go passive
                std::cout << "I: primary (passive) is restarting, ready active" << std::endl;
                m_state = STATE_ACTIVE;
            } else if (m_event == PEER_BACKUP) {
                //  Peer is restarting - become active, peer will go passive
                std::cout << "I: backup (passive) is restarting, ready active" << std::endl;
                m_state = STATE_ACTIVE;
            } else if (m_event == PEER_PASSIVE) {
                //  Two passives would mean cluster would be non-responsive
                std::cout << "E: fatal error - dual passives, aborting" << std::endl;
                exception = true;
            } else if (m_event == CLIENT_REQUEST) {
                //  Peer becomes active if timeout has passed
                //  It's the client request that triggers the failover
                assert(m_peer_expiry > 0);
                if (s_clock() >= m_peer_expiry) {
                    std::cout << "I: failover successful, ready active" << std::endl;
                    m_state = STATE_ACTIVE;
                } else {
                    //  If peer is alive, reject connections
                    exception = true;
                }
            }
        }
        return exception;
    }

    void set_state(state_t state) {
        m_state = state;
    }

    state_t get_state() {
        return m_state;
    }

    void set_peer_expiry(int64_t expiry) {
        m_peer_expiry = expiry;
    }

private:
    state_t m_state;              //  Current state
    event_t m_event;              //  Current event
    int64_t m_peer_expiry;        //  When peer is considered 'dead', milliseconds
};

int main(int argc, char *argv []) {
    //  Arguments can be either of:
    //      -p  primary server, at tcp://localhost:5001
    //      -b  backup server, at tcp://localhost:5002
    
    zmq::context_t context(1);
    zmq::socket_t statepub(context, ZMQ_PUB);
    zmq::socket_t statesub(context, ZMQ_SUB);
    statesub.set(zmq::sockopt::subscribe, "");
    zmq::socket_t frontend(context, ZMQ_ROUTER);
    bstar fsm;

    if (argc == 2 && strcmp(argv[1], "-p") == 0) {
        std::cout << "I: Primary active, waiting for backup (passive)" << std::endl;
        frontend.bind("tcp://*:5001");
        statepub.bind("tcp://*:5003");
        statesub.connect("tcp://localhost:5004");
        fsm.set_state(STATE_PRIMARY);
    } else if (argc == 2 && strcmp(argv[1], "-b") == 0) {
        std::cout << "I: Backup passive, waiting for primary (active)" << std::endl;
        frontend.bind("tcp://*:5002");
        statepub.bind("tcp://*:5004");
        statesub.connect("tcp://localhost:5003");
        fsm.set_state(STATE_BACKUP);
    } else {
        std::cout << "Usage: bstarsrv { -p | -b }" << std::endl;
        return 0;
    }
    //  .split handling socket input
    //  We now process events on our two input sockets, and process these
    //  events one at a time via our finite-state machine. Our "work" for
    //  a client request is simply to echo it back:

    //  Set timer for next outgoing state message
    int64_t send_state_at = s_clock() + HEARTBEAT;

    s_catch_signals(); // catch SIGINT and SIGTERM
    while(!s_interrupted) {
        zmq::pollitem_t items [] = {
            { frontend, 0, ZMQ_POLLIN, 0 },
            { statesub, 0, ZMQ_POLLIN, 0 }
        };
        int time_left = (int) (send_state_at - s_clock());
        if (time_left < 0)
            time_left = 0;
        try {
            zmq::poll(items, 2, time_left * ZMQ_POLL_MSEC);
        } catch (zmq::error_t &e) {
            break;              //  Interrupted
        }

        if (items[0].revents & ZMQ_POLLIN) {
            // Have client request, process it
            zmsg msg;
            msg.recv(frontend);
            if (msg.parts() == 0)
                break;          //  Ctrl-C

            if (fsm.state_machine(CLIENT_REQUEST) == false) {
                //  Answer client by echoing request back
                msg.send(frontend);
            }
        }
        if (items[1].revents & ZMQ_POLLIN) {
            //  Have state from our peer, execute as event
            std::string message = s_recv(statesub);
            std::cout << "I: received state msg:" << message << std::endl;
            event_t event = (event_t)std::stoi(message); // peer's state is our event
            if (fsm.state_machine(event) == true) {
                break;          //  Error, exit
            }
            fsm.set_peer_expiry(s_clock() + 2 * HEARTBEAT);
        }
        //  If we timed out, send state to peer
        if (s_clock() >= send_state_at) {
            std::string state = std::to_string(fsm.get_state());
            std::cout << "sending state:" << state << std::endl;
            s_send(statepub, state);
            // std::cout << "error: " << zmq_strerror(zmq_errno()) << std::endl;
            send_state_at = s_clock() + HEARTBEAT;
        }
    }
    if (s_interrupted) {
        std::cout << "W: interrupt received, shutting down..." << std::endl;
    }
    return 0;
}