#!/usr/bin/perl
=pod

Reading from multiple sockets

This version uses a simple recv loop

Author: Daisuke Maki (lestrrat)
Original version Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZMQ::LibZMQ2;
use ZMQ::Constants qw(ZMQ_PULL ZMQ_SUB ZMQ_SUBSCRIBE ZMQ_NOBLOCK);
use Time::HiRes qw/nanosleep/;

use constant NSECS_PER_MSEC => 1_000_000;

# Prepare our context and sockets
my $context = zmq_init();

# Connect to task ventilator
my $receiver = zmq_socket($context, ZMQ_PULL);
zmq_connect($receiver, 'tcp://localhost:5557');

# Connect to weather server
my $subscriber = zmq_socket($context, ZMQ_SUB);
zmq_connect($subscriber, 'tcp://localhost:5556');
zmq_setsockopt($subscriber, ZMQ_SUBSCRIBE, '10001 ');

# Process messages from both sockets
# We prioritize traffic from the task ventilator
while (1) {
    # Process any waiting tasks
    while (1) {
        my $task = zmq_recv($receiver, ZMQ_NOBLOCK);
        last unless defined $task;
    }
    # Process any waiting weather updates
    while (1) {
        my $update = zmq_recv($subscriber, ZMQ_NOBLOCK);
        last unless defined $update;
    }
    # No activity, so sleep for 1 msec
    nanosleep NSECS_PER_MSEC;
}
