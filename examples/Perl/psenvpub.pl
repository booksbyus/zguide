#!/usr/bin/perl
=pod

Pubsub envelope publisher

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZeroMQ qw/:all/;

# Prepare our context and publisher
my $context = ZeroMQ::Context->new();
my $publisher = $context->socket(ZMQ_PUB);
$publisher->bind('tcp://*:5563');

while (1) {
    # Write two messages, each with an envelope and content
    $publisher->send('A', ZMQ_SNDMORE);
    $publisher->send("We don't want to see this");
    $publisher->send('B', ZMQ_SNDMORE);
    $publisher->send("We would like to see this");
    sleep (1);
}
