#!/usr/bin/perl
=pod

Weather update server

Binds PUB socket to tcp://*:5556

Publishes random weather updates

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZeroMQ qw/:all/;

sub within {
    my ($upper) = @_;

    return int(rand($upper)) + 1;
}

# Prepare our context and publisher
my $context = ZeroMQ::Context->new();
my $publisher = $context->socket(ZMQ_PUB);
$publisher->bind('tcp://*:5556');
$publisher->bind('ipc://weather.ipc');

# Initialize random number generator
srand();
while (1) {
    # Get values that will fool the boss
    my $zipcode     = within(100_000);
    my $temperature = within(215) - 80;
    my $relhumidity = within(50) + 10;

    # Send message to all subscribers
    my $update = sprintf('%05d %d %d', $zipcode, $temperature, $relhumidity);
    $publisher->send($update);
}
