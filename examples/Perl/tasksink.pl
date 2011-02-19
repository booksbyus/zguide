#!/usr/bin/perl
=pod

Task ventilator

Binds PUSH socket to tcp://localhost:5557

Sends batch of tasks to workers via that socket

Based on examples/C/taskvent.c; translated to Perl by darksuji

=cut

use strict;
use warnings;
use feature ':5.10';

use ZeroMQ qw/:all/;
use Time::HiRes qw/time/;
use English qw/-no_match_vars/;

use constant MSECS_PER_SEC => 1000;

# Prepare our context and socket
my $context = ZeroMQ::Context->new();
my $receiver = $context->socket(ZMQ_PULL);
$receiver->bind('tcp://*:5558');

# Wait for start of batch
$receiver->recv();

# Start our clock now
my $tstart = time;

# Process 100 confirmations
my $task_count = 0;
$OUTPUT_AUTOFLUSH = 1;
while ($task_count++ < 100) {
    $receiver->recv();
    use integer;
    print '' . (($task_count / 10) * 10 == $task_count) ? ':' : '.';
}
# Calculate and report duration of batch
my $tend = time;

my $tdiff = $tend - $tstart;
my $total_msec = $tdiff * MSECS_PER_SEC;
say "Total elapsed time: $total_msec msec";
