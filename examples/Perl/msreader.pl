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

use ZMQ::LibZMQ3;
use ZMQ::Constants qw(ZMQ_PULL ZMQ_SUB ZMQ_SUBSCRIBE ZMQ_DONTWAIT);
use zhelpers;

use constant MAX_MSGLEN => 255;

# Prepare our context and sockets
my $context = zmq_init();

# Connect to task ventilator
my $receiver = zmq_socket($context, ZMQ_PULL);
zmq_connect($receiver, 'tcp://localhost:5557');

# Connect to weather server
my $subscriber = zmq_socket($context, ZMQ_SUB);
zmq_connect($subscriber, 'tcp://localhost:5556');
zmq_setsockopt($subscriber, ZMQ_SUBSCRIBE, '10001');

# Process messages from both sockets
# We prioritize traffic from the task ventilator
while (1) {
    # Process any waiting tasks
    while (1) {
        my $buf;
        my $size = zmq_recv($receiver, $buf, MAX_MSGLEN, ZMQ_DONTWAIT);
        if ($size != -1) {
            warn "Process task";
        }
        else {
            last;
        }
    }
    # Process any waiting weather updates
    while (1) {
        my $buf;
        my $size = zmq_recv($subscriber, $buf, MAX_MSGLEN, ZMQ_DONTWAIT);
        if ($size != -1) {
            warn "Process weather update";
        }
        else {
            last;
        }
    }
    # No activity, so sleep for 1 msec
    s_sleep(1);
}
