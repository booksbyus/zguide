#!/usr/bin/perl
=pod

Hello World server

Connects REP socket to tcp://*:5560

Expects "Hello" from client, replies with "World"

Author: Daisuke Maki (lestrrat)
Original version Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZMQ::LibZMQ3;
use ZMQ::Constants qw(ZMQ_REP);
use zhelpers;

my $context = zmq_init();

# Socket to talk to clients
my $responder = zmq_socket($context, ZMQ_REP);
zmq_connect($responder, 'tcp://localhost:5560');

while (1) {
    # Wait for next request from client
    my $string = s_recv($responder);
    say "Received request: [$string]";

    # Do some 'work'
    sleep (1);

    # Send reply back to client
    s_send($responder, 'World');
}
