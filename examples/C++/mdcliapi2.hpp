/*  =====================================================================
    mdcliapi2.c

    Majordomo Protocol Client API (async version)
    Implements the MDP/Worker spec at http://rfc.zeromq.org/spec:7.

    ---------------------------------------------------------------------
    Copyright (c) 1991-2011 iMatix Corporation <www.imatix.com>
    Copyright other contributors as noted in the AUTHORS file.

    This file is part of the ZeroMQ Guide: http://zguide.zeromq.org

    This is free software; you can redistribute it and/or modify it under
    the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation; either version 3 of the License, or (at
    your option) any later version.

    This software is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this program. If not, see
    <http://www.gnu.org/licenses/>.
    =====================================================================

        Andreas Hoelzlwimmer <andreas.hoelzlwimmer@fh-hagenberg.at>
*/

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
       s_version_assert (2, 1);

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
       zmq::pollitem_t items [] = { { *m_client, 0, ZMQ_POLLIN, 0 } };
       zmq::poll (items, 1, m_timeout * 1000);

       //  If we got a reply, process it
       if (items [0].revents & ZMQ_POLLIN) {
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
