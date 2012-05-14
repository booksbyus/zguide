#!/usr/bin/perl
=pod

Simple message queuing broker

Same as request-reply broker but using QUEUE device

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZeroMQ qw/:all/;
use ZeroMQ::Raw qw/zmq_device/;

my $context = ZeroMQ::Context->new();

# Socket facing clients
my $frontend = $context->socket(ZMQ_ROUTER);
$frontend->bind('tcp://*:5559');

# Socket facing services
my $backend = $context->socket(ZMQ_DEALER);
$backend->bind('tcp://*:5560');

# Start built-in device
# Having to send raw sockets here is an infelicity in ZeroMQ 0.09
zmq_device(ZMQ_QUEUE, $frontend->socket, $backend->socket);

# We never get here...
