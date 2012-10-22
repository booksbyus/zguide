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

use ZMQ::LibZMQ2;
use ZMQ::Constants qw(ZMQ_PULL ZMQ_PUSH);
use Time::HiRes qw/nanosleep/;
use English qw/-no_match_vars/;

use constant NSECS_PER_MSEC => 1000000;

my $context = zmq_init();

# Socket to receive messages on
my $receiver = zmq_socket($context, ZMQ_PULL);
zmq_connect($receiver, 'tcp://localhost:5557');

# Socket to send messages to
my $sender = zmq_socket($context, ZMQ_PUSH);
zmq_connect($sender, 'tcp://localhost:5558');

# Process tasks forever
while (1) {
    my $string = zmq_msg_data(zmq_recv($receiver));
    my $time = $string * NSECS_PER_MSEC;
    # Simple progress indicator for the viewer
    STDOUT->printflush("$string.");

    # Do the work
    nanosleep $time;

    # Send results to sink
    zmq_send($sender, '');
}
