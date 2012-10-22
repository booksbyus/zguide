#!/usr/bin/perl
=pod

Weather update client

Connects SUB socket to tcp://localhost:5556

Collects weather updates and finds avg temp in zipcode

Author: Daisuke Maki (lestrrat)
Original Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZMQ::LibZMQ2;
use ZMQ::Constants qw(ZMQ_SUB ZMQ_SUBSCRIBE);

my $context = zmq_init();

# Socket to talk to server
say 'Collecting updates from weather server...';
my $subscriber = zmq_socket($context, ZMQ_SUB);
zmq_connect($subscriber, 'tcp://localhost:5556');

# Subscribe to zipcode, default is NYC, 10001
my $filter = @ARGV ? $ARGV[0] : '10001 ';
zmq_setsockopt($subscriber, ZMQ_SUBSCRIBE, $filter);

# Process 100 updates
my $total_temp = 0;
my $update_count = 100;
for (1 .. $update_count) {
    my $string = zmq_msg_data(zmq_recv($subscriber));
    my ($zipcode, $temperature, $relhumidity) = split(/ /, $string);
    $total_temp += $temperature;
}

say "Average temperature for zipcode '$filter' was ".
    int($total_temp / $update_count);
