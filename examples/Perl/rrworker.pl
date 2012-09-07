#!/usr/bin/perl
=pod

Hello World server

Connects REP socket to tcp://*:5560

Expects "Hello" from client, replies with "World"

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZeroMQ qw/:all/;

my $context = ZeroMQ::Context->new();

# Socket to talk to clients
my $responder = $context->socket(ZMQ_REP);
$responder->connect('tcp://localhost:5560');

while (1) {
    # Wait for next request from client
    my $string = $responder->recv()->data;
    say "Received request: [$string]";

    # Do some 'work'
    sleep (1);

    # Send reply back to client
    $responder->send('World');
}
