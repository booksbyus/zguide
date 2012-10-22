#!/usr/bin/perl
=pod

Hello World client

Connects REQ socket to tcp://localhost:5555

Sends "Hello" to server, expects "World" back

Author: Daisuke Maki (lestrrat)
Original version Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZMQ::LibZMQ2;
use ZMQ::Constants qw(ZMQ_REQ);

my $context = zmq_init();

# Socket to talk to server
say 'Connecting to hello world server...';
my $requester = zmq_socket($context, ZMQ_REQ);
zmq_connect($requester, 'tcp://localhost:5555');

for my $request_nbr (0..9) {
    say "Sending request $request_nbr...";
    zmq_send($requester, 'Hello');
    my $reply = zmq_recv($requester);
    say "Received reply $request_nbr: [". zmq_msg_data($reply) .']';
}
