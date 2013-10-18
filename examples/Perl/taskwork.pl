#!/usr/bin/perl
=pod

Task worker

Connects PULL socket to tcp://localhost:5557

Collects workloads from ventilator via that socket

Connects PUSH socket to tcp://localhost:5558

Sends results to sink via that socket

Author: Daisuke Maki (lestrrat)
Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use IO::Handle;

use ZMQ::LibZMQ3;
use ZMQ::Constants qw(ZMQ_PULL ZMQ_PUSH);
use English qw/-no_match_vars/;
use zhelpers;


my $context = zmq_init();

# Socket to receive messages on
my $receiver = zmq_socket($context, ZMQ_PULL);
zmq_connect($receiver, 'tcp://localhost:5557');

# Socket to send messages to
my $sender = zmq_socket($context, ZMQ_PUSH);
zmq_connect($sender, 'tcp://localhost:5558');

# Process tasks forever
while (1) {
    my $string = s_recv($receiver);
    # Simple progress indicator for the viewer
    STDOUT->printflush(".");

    # Do the work
    s_sleep($string);

    # Send results to sink
    s_send($sender, '');
}
