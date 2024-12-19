#include <zmqpp/zmqpp.hpp>

//  We send state information every this often
//  If peer doesn't respond in two heartbeats, it is 'dead'
#define BSTAR_HEARTBEAT 1000  //  In msecs

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

class bstar_t {
public:
    bstar_t() = delete;
    bstar_t(bool primary, std::string local, std::string remote) {
        this->ctx = new zmqpp::context_t();
        this->loop = new zmqpp::loop();
        this->m_state = primary ? STATE_PRIMARY : STATE_BACKUP;

        // Create publisher for state going to peer
        this->statepub = new zmqpp::socket_t(*this->ctx, zmqpp::socket_type::pub);
        this->statepub->bind(local);

        // Create subscriber for state coming from peer
        this->statesub = new zmqpp::socket_t(*this->ctx, zmqpp::socket_type::sub);
        this->statesub->subscribe("");
        this->statesub->connect(remote);

        // Set-up basic reactor events
        this->loop->add(*this->statesub, std::bind(&bstar_t::s_recv_state, this));
        this->loop->add(std::chrono::milliseconds(BSTAR_HEARTBEAT), 0, std::bind(&bstar_t::s_send_state, this));
    }

    ~bstar_t() {
        if (statepub) delete statepub;
        if (statesub) delete statesub;
        if (loop) delete loop;
        if (ctx) delete ctx;
    }

    bstar_t(const bstar_t &) = delete;
    bstar_t &operator=(const bstar_t &) = delete;

    bstar_t(bstar_t &&src) = default;
    bstar_t &operator=(bstar_t &&src) = default;

    void set_state(state_t state) {
        m_state = state;
    }

    state_t get_state() {
        return m_state;
    }

    void set_peer_expiry(int64_t expiry) {
        m_peer_expiry = expiry;
    }

    void set_voter(std::function<void(zmqpp::loop*, zmqpp::socket_t *socket, void* args)> fn, void *arg) {
        voter_fn = fn;
        voter_arg = arg;
    }

    void set_new_active(std::function<void(zmqpp::loop*, zmqpp::socket_t *socket, void* args)> fn, void *arg) {
        active_fn = fn;
        active_arg = arg;
    }

    void set_new_passive(std::function<void(zmqpp::loop*, zmqpp::socket_t *socket, void* args)> fn, void *arg) {
        passive_fn = fn;
        passive_arg = arg;
    }

    zmqpp::loop *get_loop() {
        return loop;
    }

    //  Binary Star finite state machine (applies event to state)
    //  Returns true if there was an exception, false if event was valid.
    bool execute_fsm(event_t event) {
        m_event = event;

        bool exception = false;

        //  These are the PRIMARY and BACKUP states; we're waiting to become
        //  ACTIVE or PASSIVE depending on events we get from our peer:
        if (m_state == STATE_PRIMARY) {
            if (m_event == PEER_BACKUP) {
                std::cout << "I: connected to backup (passive), ready active" << std::endl;
                m_state = STATE_ACTIVE;
                if (active_fn) {
                    active_fn(loop, nullptr, active_arg);
                }
            } else if (m_event == PEER_ACTIVE) {
                std::cout << "I: connected to backup (active), ready passive" << std::endl;
                m_state = STATE_PASSIVE;
                if (passive_fn) {
                    passive_fn(loop, nullptr, passive_arg);
                }
            }
            //  Accept client connections
        } else if (m_state == STATE_BACKUP) {
            if (m_event == PEER_ACTIVE) {
                std::cout << "I: connected to primary (active), ready passive" << std::endl;
                m_state = STATE_PASSIVE;
                if (passive_fn) {
                    passive_fn(loop, nullptr, passive_arg);
                }
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
                auto now = std::chrono::system_clock::now();
                if (std::chrono::duration_cast<std::chrono::milliseconds>(now.time_since_epoch()).count() >= m_peer_expiry) {
                    std::cout << "I: failover successful, ready active" << std::endl;
                    m_state = STATE_ACTIVE;
                } else {
                    //  If peer is alive, reject connections
                    exception = true;
                }
            }
            // Call state change handler if necessary
            if (m_state == STATE_ACTIVE && active_fn) {
                active_fn(loop, nullptr, active_arg);
            }
        }
        return exception;
    }

    void update_peer_expiry() {
        m_peer_expiry = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count() + 2 * BSTAR_HEARTBEAT;
    }

    static bool s_send_state(bstar_t *self) {
        zmqpp::message_t msg;
        msg << static_cast<int>(self->m_state);
        std::cout << "I: publishing state " << self->m_state << std::endl;
        self->statepub->send(msg);
        return true;
    }

    static bool s_recv_state(bstar_t *self) {
        zmqpp::message_t msg;
        bool rc = self->statesub->receive(msg);
        if (rc) {
            int state;
            msg >> state;
            self->m_event = static_cast<event_t>(state);
            self->update_peer_expiry();
        }
        bool exception = self->execute_fsm(self->m_event);
        return !exception;
    }

    static bool s_voter_ready(bstar_t *self, zmqpp::socket_t *socket) {
        self->m_event = CLIENT_REQUEST;
        if (self->execute_fsm(self->m_event) == false) {
            if (self->voter_fn) {
                self->voter_fn(self->loop, socket, self->voter_arg);
            }
        } else {
            //  Destroy waiting message, no-one to read it
            zmqpp::message_t msg;
            socket->receive(msg);
        }
        return true;
    }
    
    //  .split voter method
    //  This method registers a client voter socket. Messages received
    //  on this socket provide the CLIENT_REQUEST events for the Binary Star
    //  FSM and are passed to the provided application handler. We require
    //  exactly one voter per {{bstar}} instance:

    int register_voter(std::string endpoint, zmqpp::socket_type type, std::function<void(zmqpp::loop*, zmqpp::socket_t *socket, void* args)> fn, void *arg) {
        zmqpp::socket_t *socket = new zmqpp::socket_t(*ctx, type);
        socket->bind(endpoint);
        assert(!voter_fn);
        voter_fn = fn;
        voter_arg = arg;
        loop->add(*socket, std::bind(&bstar_t::s_voter_ready, this, socket));
        return 0;
    }

    int start() {
        assert(voter_fn);
        update_peer_expiry();
        loop->start();
        return 0;
    }

private:
    zmqpp::context_t *ctx;  // Our context
    zmqpp::loop *loop;     // Reactor loop
    zmqpp::socket_t *statepub; // State publisher
    zmqpp::socket_t *statesub; // State subscriber
    state_t m_state;              //  Current state
    event_t m_event;              //  Current event
    int64_t m_peer_expiry;        //  When peer is considered 'dead', milliseconds
    std::function<void(zmqpp::loop*, zmqpp::socket_t *socket, void* args)> voter_fn;    //  Voting socket handler
    void *voter_arg;            //  Arguments for voting handler
    std::function<void(zmqpp::loop*, zmqpp::socket_t *socket, void* args)> active_fn;        //  Call when become active
    void *active_arg;           //  Arguments for handler
    std::function<void(zmqpp::loop*, zmqpp::socket_t *socket, void* args)> passive_fn;         //  Call when become passive
    void *passive_arg;            //  Arguments for handler
};