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

#ifndef __ZHELPERS_H_INCLUDED__
#define __ZHELPERS_H_INCLUDED__

//  Include a bunch of headers that we will need in the examples

#include <zmq.h>

#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>
#include <assert.h>

//  Receive 0MQ string from socket and convert into C string
static char *
s_recv (void *socket) {
    zmq_msg_t message;
    zmq_msg_init (&message);
    assert (zmq_recv (socket, &message, 0) == 0);
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
    zmq_msg_init_data (&message, string, strlen (string), NULL, NULL);
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
    zmq_msg_init_data (&message, string, strlen (string), NULL, NULL);
    rc = zmq_send (socket, &message, ZMQ_SNDMORE);
    zmq_msg_close (&message);
    assert (!rc);
    return (rc);
}

#endif
