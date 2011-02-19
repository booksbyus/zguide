#!/usr/bin/perl
=pod

Task worker

Connects PULL socket to tcp://localhost:5557

Collects workloads from ventilator via that socket

Connects PUSH socket to tcp://localhost:5558

Sends results to sink via that socket

Based on examples/C/taskwork.c; translated to Perl by darksuji

=cut

use strict;
use warnings;
use feature ':5.10';

use ZeroMQ qw/:all/;
use Time::HiRes qw/nanosleep/;
use English qw/-no_match_vars/;

use constant NSECS_PER_MSEC => 1000000;

my $context = ZeroMQ::Context->new();

# Socket to receive messages on
my $receiver = $context->socket(ZMQ_PULL);
$receiver->connect('tcp://localhost:5557');

# Socket to send messages to
my $sender = $context->socket(ZMQ_PUSH);
$sender->connect('tcp://localhost:5558');

# Process tasks forever
$OUTPUT_AUTOFLUSH = 1;
while (1) {
    my $string = $receiver->recv()->data;
    my $time = $string * NSECS_PER_MSEC;
    # Simple progress indicator for the viewer
    print "$string.";

    # Do the work
    nanosleep $time;

    # Send results to sink
    $sender->send('');
}
