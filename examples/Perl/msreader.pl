#!/usr/bin/perl
=pod

Reading from multiple sockets

This version uses a simple recv loop

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZeroMQ qw/:all/;
use Time::HiRes qw/nanosleep/;
use English qw/-no_match_vars/;

use constant NSECS_PER_MSEC => 1_000_000;

# Prepare our context and sockets
my $context = ZeroMQ::Context->new();

# Connect to task ventilator
my $receiver = $context->socket(ZMQ_PULL);
$receiver->connect('tcp://localhost:5557');

# Connect to weather server
my $subscriber = $context->socket(ZMQ_SUB);
$subscriber->connect('tcp://localhost:5556');
$subscriber->setsockopt(ZMQ_SUBSCRIBE, '10001 ');

# Process messages from both sockets
# We prioritize traffic from the task ventilator
while (1) {
    # Process any waiting tasks
    while (1) {
        my $task = $receiver->recv(ZMQ_NOBLOCK);
        last unless defined $task;
    }
    # Process any waiting weather updates
    while (1) {
        my $update = $subscriber->recv(ZMQ_NOBLOCK);
        last unless defined $update;
    }
    # No activity, so sleep for 1 msec
    nanosleep NSECS_PER_MSEC;
}
