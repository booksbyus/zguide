#!/usr/bin/perl
=pod

Hello World server

Binds REP socket to tcp://*:5555

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
$responder->bind('tcp://*:5555');

while (1) {
    # Wait for the next request from client
    my $request = $responder->recv();
    say 'Received request: ['. $request->data .']';

    # Do some 'work'
    sleep (1);

    # Send reply back to client
    $responder->send('World');
}
