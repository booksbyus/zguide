#ifndef __MDCLIAPI_HPP_INCLUDED__
#define __MDCLIAPI_HPP_INCLUDED__

#include "zmsg.hpp"
#include "mdp.h"

//  Structure of our class
//  We access these properties only via class methods

class mdcli {
public:

   //  ---------------------------------------------------------------------
   //  Constructor

   mdcli (std::string broker, int verbose)
   {
       s_version_assert (4, 0);

       m_broker = broker;
       m_context = new zmq::context_t (1);
       m_verbose = verbose;
       m_timeout = 2500;           //  msecs
       m_client = 0;

       s_catch_signals ();
       connect_to_broker ();
   }


   //  ---------------------------------------------------------------------
   //  Destructor

   virtual
   ~mdcli ()
   {
       delete m_client;
       delete m_context;
   }


   //  ---------------------------------------------------------------------
   //  Connect or reconnect to broker

   void connect_to_broker ()
   {
       if (m_client) {
           delete m_client;
       }
       m_client = new zmq::socket_t (*m_context, ZMQ_DEALER);
       int linger = 0;
       m_client->setsockopt (ZMQ_LINGER, &linger, sizeof (linger));
       s_set_id(*m_client);
       m_client->connect (m_broker.c_str());
       if (m_verbose)
           s_console ("I: connecting to broker at %s...", m_broker.c_str());
   }


   //  ---------------------------------------------------------------------
   //  Set request timeout

   void
   set_timeout (int timeout)
   {
       m_timeout = timeout;
   }


   //  ---------------------------------------------------------------------
   //  Send request to broker
   //  Takes ownership of request message and destroys it when sent.

   int
   send (std::string service, zmsg *&request_p)
   {
       assert (request_p);
       zmsg *request = request_p;

       //  Prefix request with protocol frames
       //  Frame 0: empty (REQ emulation)
       //  Frame 1: "MDPCxy" (six bytes, MDP/Client x.y)
       //  Frame 2: Service name (printable string)
       request->push_front ((char*)service.c_str());
       request->push_front ((char*)MDPC_CLIENT);
       request->push_front ((char*)"");
       if (m_verbose) {
           s_console ("I: send request to '%s' service:", service.c_str());
           request->dump ();
       }
       request->send (*m_client);
       return 0;
   }


   //  ---------------------------------------------------------------------
   //  Returns the reply message or NULL if there was no reply. Does not
   //  attempt to recover from a broker failure, this is not possible
   //  without storing all unanswered requests and resending them all...

   zmsg *
   recv ()
   {
       //  Poll socket for a reply, with timeout
       zmq::pollitem_t items[] = {
           { *m_client, 0, ZMQ_POLLIN, 0 } };
       zmq::poll (items, 1, m_timeout);

       //  If we got a reply, process it
       if (items[0].revents & ZMQ_POLLIN) {
           zmsg *msg = new zmsg (*m_client);
           if (m_verbose) {
               s_console ("I: received reply:");
               msg->dump ();
           }
           //  Don't try to handle errors, just assert noisily
           assert (msg->parts () >= 4);

           assert (msg->pop_front ().length() == 0);  // empty message

           std::basic_string<unsigned char> header = msg->pop_front();
           assert (header.compare((unsigned char *)MDPC_CLIENT) == 0);

           std::basic_string<unsigned char> service = msg->pop_front();
           assert (service.compare((unsigned char *)service.c_str()) == 0);

           return msg;     //  Success
       }
       if (s_interrupted)
           std::cout << "W: interrupt received, killing client..." << std::endl;
       else
       if (m_verbose)
           s_console ("W: permanent error, abandoning request");

       return 0;
   }

private:
   std::string m_broker;
   zmq::context_t * m_context;
   zmq::socket_t * m_client;     //  Socket to broker
   int m_verbose;                //  Print activity to stdout
   int m_timeout;                //  Request timeout
};

#endif
