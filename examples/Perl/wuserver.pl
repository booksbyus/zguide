#!/usr/bin/perl
=pod

Weather update server

Binds PUB socket to tcp://*:5556

Publishes random weather updates

Author: Daisuke Maki (lestrrat)
Original Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZMQ::LibZMQ2;
use ZMQ::Constants qw(ZMQ_PUB);

sub within {
    my ($upper) = @_;

    return int(rand($upper)) + 1;
}

# Prepare our context and publisher
my $context = zmq_init();
my $publisher = zmq_socket($context, ZMQ_PUB);
zmq_bind($publisher, 'tcp://*:5556');
zmq_bind($publisher, 'ipc://weather.ipc');

# Initialize random number generator
srand();
while (1) {
    # Get values that will fool the boss
    my $zipcode     = within(100_000);
    my $temperature = within(215) - 80;
    my $relhumidity = within(50) + 10;

    # Send message to all subscribers
    my $update = sprintf('%05d %d %d', $zipcode, $temperature, $relhumidity);
    zmq_send($publisher, $update);
}
