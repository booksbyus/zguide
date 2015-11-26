#!/usr/bin/perl
=pod

Pubsub envelope subscriber

Author: Daisuke Maki (lestrrat)
Original version Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZMQ::LibZMQ3;
use ZMQ::Constants qw(ZMQ_SUB ZMQ_SUBSCRIBE);
use zhelpers;

# Prepare our context and subscriber
my $context = zmq_init();
my $subscriber = zmq_socket($context, ZMQ_SUB);
zmq_connect($subscriber, 'tcp://localhost:5563');
zmq_setsockopt($subscriber, ZMQ_SUBSCRIBE, 'B');

while (1) {
    # Read envelope with address
    my $address = s_recv($subscriber);
    # Read message contents
    my $contents = s_recv($subscriber);
    say "[$address] $contents";
}
