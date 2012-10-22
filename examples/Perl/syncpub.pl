#!/usr/bin/perl
=pod

Synchronized publisher

Author: Daisuke Maki (lestrrat)
Original version Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZMQ::LibZMQ2;
use ZMQ::Constants qw(ZMQ_PUB ZMQ_REP);

# We wait for 10 subscribers
use constant SUBSCRIBERS_EXPECTED => 10;

my $context = zmq_init();

# Socket to talk to clients
my $publisher = zmq_socket($context, ZMQ_PUB);
zmq_bind($publisher, 'tcp://*:5561');

# Socket to receive signals
my $syncservice = zmq_socket($context, ZMQ_REP);
zmq_bind($syncservice, 'tcp://*:5562');

# Get synchronization from subscribers
for (1 .. SUBSCRIBERS_EXPECTED) {
    # - wait for synchronization request
    zmq_recv($syncservice);
    # - send synchronization reply
    zmq_send($syncservice, '');
}
# Now broadcast exactly 1M updates followed by END
for (1 .. 1_000_000) {
    zmq_send($publisher, 'Rhubarb');
}
zmq_send($publisher, 'END');
