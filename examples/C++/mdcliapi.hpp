#ifndef __MDCLIAPI_HPP_INCLUDED__
#define __MDCLIAPI_HPP_INCLUDED__

//#include "mdcliapi.h"

#include "zmsg.hpp"
#include "mdp.h"

class mdcli {
public:

   //  ---------------------------------------------------------------------
   //  Constructor

   mdcli (std::string broker, int verbose)
   {
       assert (broker.size()!=0);
       s_version_assert (4, 0);

       m_broker = broker;
       m_context = new zmq::context_t(1);
       m_verbose = verbose;
       m_timeout = 2500;           //  msecs
       m_retries = 3;              //  Before we abandon
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
       m_client = new zmq::socket_t (*m_context, ZMQ_REQ);
       s_set_id(*m_client);
       int linger = 0;
       m_client->setsockopt(ZMQ_LINGER, &linger, sizeof (linger));
       //zmq_setsockopt (client, ZMQ_LINGER, &linger, sizeof (linger));
       m_client->connect (m_broker.c_str());
       if (m_verbose) {
           s_console ("I: connecting to broker at %s...", m_broker.c_str());
       }
   }


   //  ---------------------------------------------------------------------
   //  Set request timeout

   void
   set_timeout (int timeout)
   {
       m_timeout = timeout;
   }


   //  ---------------------------------------------------------------------
   //  Set request retries

   void
   set_retries (int retries)
   {
       m_retries = retries;
   }


   //  ---------------------------------------------------------------------
   //  Send request to broker and get reply by hook or crook
   //  Takes ownership of request message and destroys it when sent.
   //  Returns the reply message or NULL if there was no reply.

   zmsg *
   send (std::string service, zmsg *&request_p)
   {
       assert (request_p);
       zmsg *request = request_p;

       //  Prefix request with protocol frames
       //  Frame 1: "MDPCxy" (six bytes, MDP/Client x.y)
       //  Frame 2: Service name (printable string)
       request->push_front((char*)service.c_str());
       request->push_front((char*)MDPC_CLIENT);
       if (m_verbose) {
           s_console ("I: send request to '%s' service:", service.c_str());
           request->dump();
       }

       int retries_left = m_retries;
       while (retries_left && !s_interrupted) {
           zmsg * msg = new zmsg(*request);
           msg->send(*m_client);

           while (!s_interrupted) {
               //  Poll socket for a reply, with timeout
               zmq::pollitem_t items [] = {
                   { *m_client, 0, ZMQ_POLLIN, 0 } };
               zmq::poll (items, 1, m_timeout);

               //  If we got a reply, process it
               if (items [0].revents & ZMQ_POLLIN) {
                   zmsg * recv_msg = new zmsg(*m_client);
                   if (m_verbose) {
                       s_console ("I: received reply:");
                       recv_msg->dump ();
                   }
                   //  Don't try to handle errors, just assert noisily
                   assert (recv_msg->parts () >= 3);

                   std::basic_string<unsigned char> header = recv_msg->pop_front();
                   assert (header.compare((unsigned char *)MDPC_CLIENT) == 0);

                   std::basic_string<unsigned char> reply_service = recv_msg->pop_front();
                   assert (reply_service.compare((unsigned char *)service.c_str()) == 0);

                   delete request;
                   return recv_msg;     //  Success
               }
               else {
                  if (--retries_left) {
                      if (m_verbose) {
                          s_console ("W: no reply, reconnecting...");
                      }
                      //  Reconnect, and resend message
                      connect_to_broker ();
                      zmsg msg (*request);
                      msg.send (*m_client);
                  }
                  else {
                      if (m_verbose) {
                          s_console ("W: permanent error, abandoning request");
                      }
                      break;          //  Give up
                  }
               }
           }
       }
       if (s_interrupted) {
           std::cout << "W: interrupt received, killing client..." << std::endl;
       }
       delete request;
       return 0;
   }

private:
   std::string m_broker;
   zmq::context_t * m_context;
   zmq::socket_t  * m_client;             //  Socket to broker
   int m_verbose;                //  Print activity to stdout
   int m_timeout;                //  Request timeout
   int m_retries;                //  Request retries
};

#endif
