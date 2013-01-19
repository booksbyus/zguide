#!/usr/bin/perl
=pod

Identity example

Author: Klaas Nijkes

=cut

use strict;
use warnings;
use 5.10.0;

use zhelpers;

my $context = zmq_init();
my $sink = zmq_socket($context, ZMQ_ROUTER);
zmq_bind($sink, "inproc://example");

# First allow 0MQ to set the identity
my $anonymous = zmq_socket($context, ZMQ_REQ);
zmq_connect($anonymous, "inproc://example");
zmq_send($anonymous, "ROUTER uses a generated UUID", -1);
s_dump($sink);

# Then set the identity ourselves
my $identified = zmq_socket($context, ZMQ_REQ);
zmq_setsockopt($identified, ZMQ_IDENTITY, "PEER2");
zmq_connect($identified, "inproc://example");
zmq_send($identified, "ROUTER socket uses REQ's socket identity");
s_dump($sink);

zmq_close($sink);
zmq_close($anonymous);
zmq_close($identified);
zmq_term($context);

exit(0);
