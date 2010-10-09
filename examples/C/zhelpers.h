/*  =========================================================================
    zhelpers.h

    Helper header file for example applications.

    -------------------------------------------------------------------------
    Copyright (c) 1991-2010 iMatix Corporation <www.imatix.com>
    Copyright other contributors as noted in the AUTHORS file.

    This file is part of the ZeroMQ Guide: http://zguide.zeromq.org

    This is free software; you can redistribute it and/or modify it under the
    terms of the GNU Lesser General Public License as published by the Free
    Software Foundation; either version 3 of the License, or (at your option)
    any later version.

    This software is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABIL-
    ITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General
    Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
    =========================================================================
*/

#ifndef __ZHELPERS_H_INCLUDED__
#define __ZHELPERS_H_INCLUDED__

//  Include a bunch of headers that we will need in the examples

#include <zmq.h>

#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>
#include <assert.h>

//  Bring Windows MSVC up to C99 scratch
#if (defined (__WINDOWS__))
    typedef unsigned long ulong;
    typedef unsigned int  uint;
    typedef __int64 int64_t;
#endif

//  Provide random number from 0..(num-1)
#define within(num) (int) ((float) (num) * random () / (RAND_MAX + 1.0))

//  Receive 0MQ string from socket and convert into C string
//  Caller must free returned string.
static char *
s_recv (void *socket) {
    zmq_msg_t message;
    zmq_msg_init (&message);
    if (zmq_recv (socket, &message, 0))
        exit (1);           //  Context terminated, exit

    int size = zmq_msg_size (&message);
    char *string = malloc (size + 1);
    memcpy (string, zmq_msg_data (&message), size);
    zmq_msg_close (&message);
    string [size] = 0;
    return (string);
}

//  Convert C string to 0MQ string and send to socket
static int
s_send (void *socket, char *string) {
    int rc;
    zmq_msg_t message;
    zmq_msg_init_size (&message, strlen (string));
    memcpy (zmq_msg_data (&message), string, strlen (string));
    rc = zmq_send (socket, &message, 0);
    assert (!rc);
    zmq_msg_close (&message);
    return (rc);
}

//  Sends string as 0MQ string, as multipart non-terminal
static int
s_sendmore (void *socket, char *string) {
    int rc;
    zmq_msg_t message;
    zmq_msg_init_size (&message, strlen (string));
    memcpy (zmq_msg_data (&message), string, strlen (string));
    rc = zmq_send (socket, &message, ZMQ_SNDMORE);
    zmq_msg_close (&message);
    assert (!rc);
    return (rc);
}

//  Receives all message parts from socket, prints neatly
//
static void
s_dump (void *socket)
{
    puts ("----------------------------------------");
    while (1) {
        //  Process all parts of the message
        zmq_msg_t message;
        zmq_msg_init (&message);
        zmq_recv (socket, &message, 0);

        //  Dump the message as text or binary
        char *data = zmq_msg_data (&message);
        int size = zmq_msg_size (&message);
        int is_text = 1;
        int char_nbr;
        for (char_nbr = 0; char_nbr < size; char_nbr++)
            if ((unsigned char) data [char_nbr] < 32
            ||  (unsigned char) data [char_nbr] > 127)
                is_text = 0;

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
        zmq_getsockopt (socket, ZMQ_RCVMORE, &more, &more_size);
        zmq_msg_close (&message);
        if (!more)
            break;      //  Last message part
    }
}

//  Set simple random printable identity on socket
//
static void
s_set_id (void *socket)
{
    char identity [10];
    sprintf (identity, "%04X-%04X", within (0x10000), within (0x10000));
    zmq_setsockopt (socket, ZMQ_IDENTITY, identity, strlen (identity));
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
