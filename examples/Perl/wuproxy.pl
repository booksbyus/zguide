#!/usr/bin/perl
=pod

Weather proxy device

Author: Daisuke Maki (lestrrat)
Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZMQ::LibZMQ2;
use ZMQ::Constants qw(ZMQ_SUB ZMQ_PUB ZMQ_SUBSCRIBE ZMQ_RCVMORE ZMQ_SNDMORE);

my $context = zmq_init();

# This is where the weather server sits
my $frontend = zmq_socket($context, ZMQ_SUB);
zmq_connect($frontend, 'tcp://192.168.55.210:5556');

# This is our public endpoint for subscribers
my $backend = zmq_socket($context, ZMQ_PUB);
zmq_bind($backend, 'tcp://10.1.1.0:8100');

# Subscribe on everything
zmq_setsockopt($frontend, ZMQ_SUBSCRIBE, '');

# Shunt messages out to our own subscribers
while (1) {
    while (1) {
        # Process all parts of the message
        my $message = zmq_recv($frontend);
        my $more = zmq_getsockopt($frontend, ZMQ_RCVMORE);
        zmq_send($backend, $message, $more ? ZMQ_SNDMORE : 0);
        last unless $more;
    }
}
