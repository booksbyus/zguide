#!/usr/bin/perl
=pod

Pubsub envelope publisher

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZMQ::LibZMQ3;
use ZMQ::Constants qw(ZMQ_PUB ZMQ_SNDMORE);
use zhelpers;

# Prepare our context and publisher
my $context = zmq_init();
my $publisher = zmq_socket($context, ZMQ_PUB);
zmq_bind($publisher, 'tcp://*:5563');

while (1) {
    # Write two messages, each with an envelope and content
    s_sendmore($publisher, 'A', ZMQ_SNDMORE);
    s_send($publisher, "We don't want to see this");
    s_sendmore($publisher, 'B', ZMQ_SNDMORE);
    s_send($publisher, "We would like to see this");
    sleep (1);
}
