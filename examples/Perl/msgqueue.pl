#!/usr/bin/perl
=pod

Simple message queuing broker

Same as request-reply broker but using QUEUE device

Author: Daisuke Maki (lestrrat)
Original version Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZMQ::LibZMQ2;
use ZMQ::Constants qw(ZMQ_DEALER ZMQ_ROUTER ZMQ_QUEUE);

my $context = zmq_init();

# Socket facing clients
my $frontend = zmq_socket($context, ZMQ_ROUTER);
zmq_bind($frontend, 'tcp://*:5559');

# Socket facing services
my $backend = zmq_socket($context, ZMQ_DEALER);
zmq_bind($backend, 'tcp://*:5560');

# Start built-in device
zmq_device(ZMQ_QUEUE, $frontend, $backend);

# We never get here...
