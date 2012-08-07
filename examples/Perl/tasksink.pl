#!/usr/bin/perl
=pod

Task sink

Binds PULL socket to tcp://localhost:5558

Collects results from workers via that socket

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>
Amend:  Sonia Hamilton <sonia@snowfrog.net>

=cut

use strict;
use warnings;
use 5.10.0;

use IO::Handle;

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
for my $task_nbr (0 .. 99) {
    $receiver->recv();
    use integer;
    if (($task_nbr / 10) * 10 == $task_nbr) {
        print ':';
    } else {
        print '.';
    }
    STDOUT->flush();
}
# Calculate and report duration of batch
my $tend = time;

my $tdiff = $tend - $tstart;
my $total_msec = $tdiff * MSECS_PER_SEC;
say "Total elapsed time: $total_msec msec";
