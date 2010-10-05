/*  =========================================================================
    zhelpers.h - ZeroMQ helpers for example applications

    Copyright (c) 1991-2010 iMatix Corporation and contributors

    This is free software; you can redistribute it and/or modify it under
    the terms of the Lesser GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    Lesser GNU General Public License for more details.

    You should have received a copy of the Lesser GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
    =========================================================================
*/

// Olivier Chamoux <olivier.chamoux@fr.thalesgroup.com>


#ifndef __ZHELPERS_HPP_INCLUDED__
#define __ZHELPERS_HPP_INCLUDED__

//  Include a bunch of headers that we will need in the examples

#include <zmq.hpp>

#include <iostream>
#include <string>
#include <sstream>

#include <sys/time.h>
#include <time.h>
#include <assert.h>
#include <pthread.h>
#include <stdlib.h>		// random()  RAND_MAX
#include <stdio.h>

#define within(num) (int) ((float) (num) * random () / (RAND_MAX + 1.0))

//  Receive 0MQ string from socket and convert into string
static std::string *
s_recv (zmq::socket_t & socket) {

    zmq::message_t message;
    socket.recv(&message);

    std::string * string = new std::string(static_cast<char*>(message.data()), message.size());

    return (string);
}

//  Convert string to 0MQ string and send to socket
static bool
s_send (zmq::socket_t & socket, const std::string & string) {

    zmq::message_t message(string.size());
    memcpy(message.data(), string.data(), string.size());

    bool rc = socket.send(message);
    return (rc);
}

//  Sends string as 0MQ string, as multipart non-terminal
static bool
s_sendmore (zmq::socket_t & socket, const std::string & string) {

    zmq::message_t message(string.size());
    memcpy(message.data(), string.data(), string.size());

    bool rc = socket.send(message, ZMQ_SNDMORE);
    return (rc);
}

//  Receives all message parts from socket, prints neatly
//
static void
s_dump (zmq::socket_t & socket)
{
    std::cout << "----------------------------------------" << std::endl;

    while (1) {
        //  Process all parts of the message

		zmq::message_t message;
        socket.recv(&message);

        //  Dump the message as text or binary
        std::string data(static_cast<char*>(message.data()));
        int size = message.size();

        bool is_text = true;

        int char_nbr;
        unsigned char byte;
        for (char_nbr = 0; char_nbr < size; char_nbr++) {
            byte = data [char_nbr];
            if (byte < 32 || byte > 127)
              is_text = false;
        }

        printf ("[%03d] ", size);

        for (char_nbr = 0; char_nbr < size; char_nbr++) {
            if (is_text)
                printf ("%c", data [char_nbr]);
            else
                printf ("%02X", (unsigned char) data [char_nbr]);
        }
        printf ("\n");

        int64_t more;           //  Multipart detection
        size_t more_size = sizeof (more);
        socket.getsockopt(ZMQ_RCVMORE, &more, &more_size);

        if (!more)
            break;      //  Last message part
    }
}

//  Set simple random printable identity on socket
//
static void
s_set_id (zmq::socket_t & socket)
{
    char identity [10];
    sprintf (identity, "%04X-%04X", within (0x10000), within (0x10000));
    socket.setsockopt(ZMQ_IDENTITY, identity, strlen (identity));
}

//  Report 0MQ version number
//
static void
s_version (void)
{
    int major, minor, patch;
    zmq_version (&major, &minor, &patch);
    printf ("Current 0MQ version is %d.%d.%d\n", major, minor, patch);
}
#endif
