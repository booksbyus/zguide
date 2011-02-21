#!/usr/bin/perl
=pod

Hello World server

Connects REP socket to tcp://*:5560

Expects "Hello" from client, replies with "World"

Based on examples/C/taskwork2.c; translated to Perl by darksuji

=cut

use strict;
use warnings;
use feature ':5.10';

use ZeroMQ qw/:all/;

my $context = ZeroMQ::Context->new();

# Socket to talk to clients
my $responder = $context->socket(ZMQ_REP);
$responder->connect('tcp://localhost:5560');

while (1) {
    # Wait for next request from client
    my $string = $responder->recv()->data;
    printf("Received request: [%s]\n", $string);

    # Do some 'work'
    sleep (1);

    # Send reply back to client
    $responder->send('World');
}
