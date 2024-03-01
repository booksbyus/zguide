#ifndef __MDWRKAPI_HPP_INCLUDED__
#define __MDWRKAPI_HPP_INCLUDED__

#include "zmsg.hpp"
#include "mdp.h"

//  Reliability parameters

//  Structure of our class
//  We access these properties only via class methods
class mdwrk {
public:

   //  ---------------------------------------------------------------------
   //  Constructor

    mdwrk (std::string broker, std::string service, int verbose): m_broker(broker), m_service(service),m_verbose(verbose)
    {
        s_version_assert (4, 0);
        m_context = new zmq::context_t (1);
        s_catch_signals ();
        connect_to_broker ();
    }

    //  ---------------------------------------------------------------------
    //  Destructor

    virtual
    ~mdwrk ()
    {
        delete m_worker;
        delete m_context;
    }


    //  ---------------------------------------------------------------------
    //  Send message to broker
    //  If no _msg is provided, creates one internally
    void send_to_broker(const char *command, std::string option, zmsg *_msg)
    {
        zmsg *msg = _msg? new zmsg(*_msg): new zmsg ();

        //  Stack protocol envelope to start of message
        if (!option.empty()) {
            msg->push_front (option.c_str());
        }
        msg->push_front (command);
        msg->push_front (k_mdpw_worker.data());
        msg->push_front ("");

        if (m_verbose) {
            s_console ("I: sending %s to broker",
                mdps_commands [(int) *command].data());
            msg->dump ();
        }
        msg->send (*m_worker);
        delete msg;
    }

    //  ---------------------------------------------------------------------
    //  Connect or reconnect to broker

    void connect_to_broker ()
    {
        if (m_worker) {
            delete m_worker;
        }
        m_worker = new zmq::socket_t (*m_context, ZMQ_DEALER);
        int linger = 0;
        m_worker->setsockopt (ZMQ_LINGER, &linger, sizeof (linger));
        s_set_id(*m_worker);
        m_worker->connect (m_broker.c_str());
        if (m_verbose)
            s_console ("I: connecting to broker at %s...", m_broker.c_str());

        //  Register service with broker
        send_to_broker (k_mdpw_ready.data(), m_service, NULL);

        //  If liveness hits zero, queue is considered disconnected
        m_liveness = n_heartbeat_liveness;
        m_heartbeat_at = s_clock () + m_heartbeat;
    }


    //  ---------------------------------------------------------------------
    //  Set heartbeat delay

    void
    set_heartbeat (int heartbeat)
    {
        m_heartbeat = heartbeat;
    }


    //  ---------------------------------------------------------------------
    //  Set reconnect delay

    void
    set_reconnect (int reconnect)
    {
        m_reconnect = reconnect;
    }

    //  ---------------------------------------------------------------------
    //  Send reply, if any, to broker and wait for next request.

    zmsg *
    recv (zmsg *&reply_p)
    {
        //  Format and send the reply if we were provided one
        zmsg *reply = reply_p;
        assert (reply || !m_expect_reply);
        if (reply) {
            assert (m_reply_to.size()!=0);
            reply->wrap (m_reply_to.c_str(), "");
            m_reply_to = "";
            send_to_broker (k_mdpw_reply.data(), "", reply);
            delete reply_p;
            reply_p = 0;
        }
        m_expect_reply = true;

        while (!s_interrupted) {
            zmq::pollitem_t items[] = {
                { *m_worker,  0, ZMQ_POLLIN, 0 } };
            zmq::poll (items, 1, m_heartbeat);

            if (items[0].revents & ZMQ_POLLIN) {
                zmsg *msg = new zmsg(*m_worker);
                if (m_verbose) {
                    s_console ("I: received message from broker:");
                    msg->dump ();
                }
                m_liveness = n_heartbeat_liveness;

                //  Don't try to handle errors, just assert noisily
                assert (msg->parts () >= 3);

                ustring empty = msg->pop_front ();
                assert (empty.compare((unsigned char *)"") == 0);
                //assert (strcmp (empty, "") == 0);
                //free (empty);

                ustring header = msg->pop_front ();
                assert (header.compare((unsigned char *)k_mdpw_worker.data()) == 0);
                //free (header);

                std::string command =(char*) msg->pop_front ().c_str();
                if (command.compare (k_mdpw_request.data()) == 0) {
                    //  We should pop and save as many addresses as there are
                    //  up to a null part, but for now, just save one...
                    m_reply_to = msg->unwrap ();
                    return msg;     //  We have a request to process
                }
                else if (command.compare (k_mdpw_heartbeat.data()) == 0) {
                    //  Do nothing for heartbeats
                }
                else if (command.compare (k_mdpw_disconnect.data()) == 0) {
                    connect_to_broker ();
                }
                else {
                    s_console ("E: invalid input message (%d)",
                          (int) *(command.c_str()));
                    msg->dump ();
                }
                delete msg;
            }
            else
            if (--m_liveness == 0) {
                if (m_verbose) {
                    s_console ("W: disconnected from broker - retrying...");
                }
                s_sleep (m_reconnect);
                connect_to_broker ();
            }
            //  Send HEARTBEAT if it's time
            if (s_clock () >= m_heartbeat_at) {
                send_to_broker (k_mdpw_heartbeat.data(), "", NULL);
                m_heartbeat_at += m_heartbeat;
            }
        }
        if (s_interrupted)
            printf ("W: interrupt received, killing worker...\n");
        return NULL;
    }

private:

    static constexpr uint32_t n_heartbeat_liveness = 3;//   3-5 is reasonable
    const std::string m_broker;
    const std::string m_service;
    zmq::context_t *m_context;
    zmq::socket_t  *m_worker{};     //  Socket to broker
    const int m_verbose;                //  Print activity to stdout

    //  Heartbeat management
    int64_t m_heartbeat_at;      //  When to send HEARTBEAT
    size_t m_liveness;            //  How many attempts left
    int m_heartbeat{2500};              //  Heartbeat delay, msecs
    int m_reconnect{2500};              //  Reconnect delay, msecs

    //  Internal state
    bool m_expect_reply{false};           //  Zero only at start

    //  Return address, if any
    std::string m_reply_to;
};

#endif
