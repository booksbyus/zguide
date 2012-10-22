#!/usr/bin/perl
=pod

Pubsub envelope subscriber

Author: Daisuke Maki (lestrrat)
Original version Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZMQ::LibZMQ2;
use ZMQ::Constants qw(ZMQ_SUB ZMQ_SUBSCRIBE);

# Prepare our context and subscriber
my $context = zmq_init();
my $subscriber = zmq_socket($context, ZMQ_SUB);
zmq_connect($subscriber, 'tcp://localhost:5563');
zmq_setsockopt($subscriber, ZMQ_SUBSCRIBE, 'B');

while (1) {
    # Read envelope with address
    my $address = zmq_msg_data(zmq_recv($subscriber));
    # Read message contents
    my $contents = zmq_msg_data(zmq_recv($subscriber));
    say "[$address] $contents";
}
