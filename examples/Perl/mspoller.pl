#!/usr/bin/perl
=pod

Reading from multiple sockets

This version uses zmq_poll()

Author: Daisuke Maki (lestrrat)
Original version Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZMQ::LibZMQ2;
use ZMQ::Constants qw(ZMQ_PULL ZMQ_SUB ZMQ_SUBSCRIBE ZMQ_POLLIN);

my $context = zmq_init();

# Connect to task ventilator
my $receiver = zmq_socket($context, ZMQ_PULL);
zmq_connect($receiver, 'tcp://localhost:5557');

# Connect to weather server
my $subscriber = zmq_socket($context, ZMQ_SUB);
zmq_connect($subscriber, 'tcp://localhost:5556');
zmq_setsockopt($subscriber, ZMQ_SUBSCRIBE, '10001');

while (1) {
# Process messages from both sockets
    zmq_poll([
        {
            socket  => $receiver,
            events  => ZMQ_POLLIN,
            callback => sub {
                warn "Process task";
            }
        }, {
            socket  => $subscriber,
            events  => ZMQ_POLLIN,
            callback => sub {
                warn "Process weather update";
            }
        },
    ]);
}
# We never get here
