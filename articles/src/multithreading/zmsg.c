/*  =========================================================================
    zmsg.h

    Multipart message class for example applications.

    Follows the ZFL class conventions and is further developed as the ZFL
    zfl_msg class.  See http://zfl.zeromq.org for more details.

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

#ifndef __ZMSG_H_INCLUDED__
#define __ZMSG_H_INCLUDED__

#include "zhelpers.h"

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _zmsg_t zmsg_t;

//  Constructor and destructor
zmsg_t *zmsg_new      (void);
void    zmsg_destroy  (zmsg_t **self_p);

//  Receive and send message, wrapping new/destroy
zmsg_t *zmsg_recv     (void *socket);
void    zmsg_send     (zmsg_t **self, void *socket);

//  Report size of message
size_t  zmsg_parts    (zmsg_t *self);

//  Read and set message body part as C string
char   *zmsg_body     (zmsg_t *self);
void    zmsg_body_set (zmsg_t *self, char *body);
void    zmsg_body_fmt (zmsg_t *self, char *format, ...);

//  Generic push/pop message part off front
void    zmsg_push     (zmsg_t *self, char *part);
char   *zmsg_pop      (zmsg_t *self);

//  Read and set message envelopes
char   *zmsg_address  (zmsg_t *self);
void    zmsg_wrap     (zmsg_t *self, char *address, char *delim);
char   *zmsg_unwrap   (zmsg_t *self);

//  Dump message to stderr, for debugging and tracing
void    zmsg_dump     (zmsg_t *self);

//  Selftest for the class
int     zmsg_test     (int verbose);

#ifdef __cplusplus
}
#endif

#endif

//  Pretty arbitrary limit on complexity of a message
#define ZMSG_MAX_PARTS  255

//  Structure of our class
//  We access these properties only via class methods

struct _zmsg_t {
    //  Part data follows message recv/send order
    unsigned char
          *_part_data [ZMSG_MAX_PARTS];
    size_t _part_size [ZMSG_MAX_PARTS];
    size_t _part_count;
};


//  --------------------------------------------------------------------------
//  Constructor

zmsg_t *
zmsg_new (void)
{
    zmsg_t
        *self;

    self = malloc (sizeof (zmsg_t));
    memset (self, 0, sizeof (zmsg_t));
    return (self);
}


//  --------------------------------------------------------------------------
//  Destructor

void
zmsg_destroy (zmsg_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        zmsg_t *self = *self_p;

        //  Free message parts, if any
        while (self->_part_count)
            free (zmsg_pop (self));

        //  Free object structure
        free (self);
        *self_p = NULL;
    }
}


//  --------------------------------------------------------------------------
//  Formats 17-byte UUID as 33-char string starting with '@'
//  Lets us print UUIDs as C strings and use them as addresses
//
static char *
s_encode_uuid (unsigned char *data)
{
    static char
        hex_char [] = "0123456789ABCDEF";

    assert (data [0] == 0);
    char *uuidstr = malloc (34);
    uuidstr [0] = '@';
    int byte_nbr;
    for (byte_nbr = 0; byte_nbr < 16; byte_nbr++) {
        uuidstr [byte_nbr * 2 + 1] = hex_char [data [byte_nbr + 1] >> 4];
        uuidstr [byte_nbr * 2 + 2] = hex_char [data [byte_nbr + 1] & 15];
    }
    uuidstr [33] = 0;
    return (uuidstr);
}


//  --------------------------------------------------------------------------
//  Formats 17-byte UUID as 33-char string starting with '@'
//  Lets us print UUIDs as C strings and use them as addresses
//
static unsigned char *
s_decode_uuid (char *uuidstr)
{
    static char
        hex_to_bin [128] = {
           -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,    /*            */
           -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,    /*            */
           -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,    /*            */
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,    /*   0..9     */
           -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,    /*   A..F     */
           -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,    /*            */
           -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,    /*   a..f     */
           -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1 };  /*            */

    assert (strlen (uuidstr) == 33);
    assert (uuidstr [0] == '@');
    unsigned char *data = malloc (17);
    int byte_nbr;
    data [0] = 0;
    for (byte_nbr = 0; byte_nbr < 16; byte_nbr++)
        data [byte_nbr + 1]
            = (hex_to_bin [uuidstr [byte_nbr * 2 + 1] & 127] << 4)
            + (hex_to_bin [uuidstr [byte_nbr * 2 + 2] & 127]);

    return (data);
}

//  --------------------------------------------------------------------------
//  Private helper function to store a single message part

static void
_set_part (zmsg_t *self, int part_nbr, unsigned char *data, size_t size)
{
    self->_part_size [part_nbr] = size;
    self->_part_data [part_nbr] = malloc (size + 1);
    memcpy (self->_part_data [part_nbr], data, size);
    //  Convert to C string if needed
    self->_part_data [part_nbr][size] = 0;
}


//  --------------------------------------------------------------------------
//  Receive message from socket
//  Creates a new message and returns it
//  Blocks on recv if socket is not ready for input

zmsg_t *
zmsg_recv (void *socket)
{
    assert (socket);

    zmsg_t *self = zmsg_new ();
    while (1) {
        zmq_msg_t message;
        zmq_msg_init (&message);
        if (zmq_recv (socket, &message, 0)) {
            if (errno != ETERM)
                printf ("E: %s\n", zmq_strerror (errno));
            exit (1);
        }
        //  We handle 0MQ UUIDs as printable strings
        unsigned char *data = zmq_msg_data (&message);
        size_t         size = zmq_msg_size (&message);
        if (size == 17 && data [0] == 0) {
            //  Store message part as string uuid
            char *uuidstr = s_encode_uuid (data);
            self->_part_size [self->_part_count] = strlen (uuidstr);
            self->_part_data [self->_part_count] = (unsigned char *) uuidstr;
            self->_part_count++;
        }
        else
            //  Store this message part
            _set_part (self, self->_part_count++, data, size);

        zmq_msg_close (&message);

        int64_t more;
        size_t more_size = sizeof (more);
        zmq_getsockopt (socket, ZMQ_RCVMORE, &more, &more_size);
        if (!more)
            break;      //  Last message part
    }
    return (self);
}


//  --------------------------------------------------------------------------
//  Send message to socket
//  Destroys message after sending

void
zmsg_send (zmsg_t **self_p, void *socket)
{
    assert (self_p);
    assert (*self_p);
    assert (socket);
    zmsg_t *self = *self_p;

    int part_nbr;
    for (part_nbr = 0; part_nbr < self->_part_count; part_nbr++) {
        //  Could be improved to use zero-copy since we destroy
        //  the message parts after sending anyhow...
        zmq_msg_t message;

        //  Unmangle 0MQ identities for writing to the socket
        unsigned char *data = self->_part_data [part_nbr];
        size_t         size = self->_part_size [part_nbr];
        if (size == 33 && data [0] == '@') {
            unsigned char *uuidbin = s_decode_uuid ((char *) data);
            zmq_msg_init_size (&message, 17);
            memcpy (zmq_msg_data (&message), uuidbin, 17);
            free (uuidbin);
        }
        else {
            zmq_msg_init_size (&message, size);
            memcpy (zmq_msg_data (&message), data, size);
        }
        assert (zmq_send (socket, &message,
            part_nbr < self->_part_count - 1? ZMQ_SNDMORE: 0) == 0);
        zmq_msg_close (&message);
    }
    zmsg_destroy (self_p);
}


//  --------------------------------------------------------------------------
//  Report size of message

size_t
zmsg_parts (zmsg_t *self)
{
    return (self->_part_count);
}


//  --------------------------------------------------------------------------
//  Return pointer to message body, if any
//  Caller should not modify the provided data

char *
zmsg_body (zmsg_t *self)
{
    assert (self);

    if (self->_part_count)
        return ((char *) self->_part_data [self->_part_count - 1]);
    else
        return (NULL);
}


//  --------------------------------------------------------------------------
//  Set message body as copy of provided string
//  If message is empty, creates a new message body

void
zmsg_body_set (zmsg_t *self, char *body)
{
    assert (self);
    assert (body);

    if (self->_part_count) {
        assert (self->_part_data [self->_part_count - 1]);
        free (self->_part_data [self->_part_count - 1]);
    }
    else
        self->_part_count = 1;

    _set_part (self, self->_part_count - 1, (void *) body, strlen (body));
}


//  --------------------------------------------------------------------------
//  Set message body using printf format
//  If message is empty, creates a new message body
//  Hard-coded to max. 255 characters for this simplified class

void
zmsg_body_fmt (zmsg_t *self, char *format, ...)
{
    char value [255 + 1];
    va_list args;

    assert (self);
    va_start (args, format);
    vsnprintf (value, 255, format, args);
    va_end (args);
    zmsg_body_set (self, value);
}


//  --------------------------------------------------------------------------
//  Push message part to front of message parts

void
zmsg_push (zmsg_t *self, char *part)
{
    assert (self);
    assert (part);
    assert (self->_part_count < ZMSG_MAX_PARTS - 1);

    //  Move part stack up one element and insert new part
    memmove (&self->_part_data [1], &self->_part_data [0],
        (ZMSG_MAX_PARTS - 1) * sizeof (unsigned char *));
    memmove (&self->_part_size [1], &self->_part_size [0],
        (ZMSG_MAX_PARTS - 1) * sizeof (size_t));
    _set_part (self, 0, (void *) part, strlen (part));
    self->_part_count += 1;
}


//  --------------------------------------------------------------------------
//  Pop message part off front of message parts
//  Caller should free returned string when finished with it

char *
zmsg_pop (zmsg_t *self)
{
    assert (self);
    assert (self->_part_count);

    //  Remove first part and move part stack down one element
    char *part = (char *) self->_part_data [0];
    memmove (&self->_part_data [0], &self->_part_data [1],
        (ZMSG_MAX_PARTS - 1) * sizeof (unsigned char *));
    memmove (&self->_part_size [0], &self->_part_size [1],
        (ZMSG_MAX_PARTS - 1) * sizeof (size_t));
    self->_part_count--;
    return (part);
}


//  --------------------------------------------------------------------------
//  Return pointer to outer message address, if any
//  Caller should not modify the provided data

char *
zmsg_address (zmsg_t *self)
{
    assert (self);

    if (self->_part_count)
        return ((char *) self->_part_data [0]);
    else
        return (NULL);
}


//  --------------------------------------------------------------------------
//  Wraps message in new address envelope
//  If delim is not null, creates two-part envelope

void
zmsg_wrap (zmsg_t *self, char *address, char *delim)
{
    assert (self);
    assert (address);

    //  Push optional delimiter and then address
    if (delim)
        zmsg_push (self, delim);
    zmsg_push (self, address);
}


//  --------------------------------------------------------------------------
//  Unwraps outer message envelope and returns address
//  Discards empty message part after address, if any
//  Caller should free returned string when finished with it

char *
zmsg_unwrap (zmsg_t *self)
{
    assert (self);

    char *address = zmsg_pop (self);
    if (*zmsg_address (self) == 0)
        free (zmsg_pop (self));
    return (address);
}


//  --------------------------------------------------------------------------
//  Dump message to stderr, for debugging and tracing

void
zmsg_dump (zmsg_t *self)
{
    int part_nbr;
    for (part_nbr = 0; part_nbr < self->_part_count; part_nbr++) {
        unsigned char *data = self->_part_data [part_nbr];
        size_t         size = self->_part_size [part_nbr];

        //  Dump the message as text or binary
        int is_text = 1;
        int char_nbr;
        for (char_nbr = 0; char_nbr < size; char_nbr++)
            if (data [char_nbr] < 32 || data [char_nbr] > 127)
                is_text = 0;

        fprintf (stderr, "[%03d] ", (int) size);
        for (char_nbr = 0; char_nbr < size; char_nbr++) {
            if (is_text)
                fprintf (stderr, "%c", data [char_nbr]);
            else
                fprintf (stderr, "%02X", (unsigned char) data [char_nbr]);
        }
        fprintf (stderr, "\n");
    }
    fflush (stderr);
}



//  --------------------------------------------------------------------------
//  Runs self test of class

int
zmsg_test (int verbose)
{
    zmsg_t
        *zmsg;

    printf (" * zmsg: ");

    //  Prepare our context and sockets
    void *context = zmq_init (1);
    void *output = zmq_socket (context, ZMQ_DEALER);
    assert (zmq_bind (output, "ipc://zmsg_selftest.ipc") == 0);
    void *input = zmq_socket (context, ZMQ_ROUTER);
    assert (zmq_connect (input, "ipc://zmsg_selftest.ipc") == 0);

    //  Test send and receive of single-part message
    zmsg = zmsg_new ();
    assert (zmsg);
    zmsg_body_set (zmsg, "Hello");
    assert (strcmp (zmsg_body (zmsg), "Hello") == 0);
    zmsg_send (&zmsg, output);
    assert (zmsg == NULL);

    zmsg = zmsg_recv (input);
    assert (zmsg_parts (zmsg) == 2);
    if (verbose)
        zmsg_dump (zmsg);
    assert (strcmp (zmsg_body (zmsg), "Hello") == 0);

    //  Test send and receive of multi-part message
    zmsg = zmsg_new ();
    zmsg_body_set (zmsg, "Hello");
    zmsg_wrap     (zmsg, "address1", "");
    zmsg_wrap     (zmsg, "address2", NULL);
    assert (zmsg_parts (zmsg) == 4);
    zmsg_send (&zmsg, output);

    zmsg = zmsg_recv (input);
    if (verbose)
        zmsg_dump (zmsg);
    assert (zmsg_parts (zmsg) == 5);
    assert (strlen (zmsg_address (zmsg)) == 33);
    free (zmsg_unwrap (zmsg));
    assert (strcmp (zmsg_address (zmsg), "address2") == 0);
    zmsg_body_fmt (zmsg, "%c%s", 'W', "orld");
    zmsg_send (&zmsg, output);

    zmsg = zmsg_recv (input);
    free (zmsg_unwrap (zmsg));
    assert (zmsg_parts (zmsg) == 4);
    assert (strcmp (zmsg_body (zmsg), "World") == 0);
    char *part;
    part = zmsg_unwrap (zmsg);
    assert (strcmp (part, "address2") == 0);
    free (part);

    //  Pull off address 1, check that empty part was dropped
    part = zmsg_unwrap (zmsg);
    assert (strcmp (part, "address1") == 0);
    assert (zmsg_parts (zmsg) == 1);
    free (part);

    //  Check that message body was correctly modified
    part = zmsg_pop (zmsg);
    assert (strcmp (part, "World") == 0);
    assert (zmsg_parts (zmsg) == 0);

    zmsg_destroy (&zmsg);
    assert (zmsg == NULL);

    printf ("OK\n");
    zmq_term (context);
    return 0;
}
