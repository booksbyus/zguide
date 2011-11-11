#!/usr/bin/perl
=pod

Task sink - design 2

Adds pub-sub flow to send kill signal to workers

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use IO::Handle;

use ZeroMQ qw/:all/;
use Time::HiRes qw/time/;
use English qw/-no_match_vars/;

use constant MSECS_PER_SEC => 1000;

my $context = ZeroMQ::Context->new();

# Socket to receive messages on
my $receiver = $context->socket(ZMQ_PULL);
$receiver->bind('tcp://*:5558');

# Socket for worker control
my $controller = $context->socket(ZMQ_PUB);
$controller->bind('tcp://*:5559');

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

# Send kill signal to workers
$controller->send('KILL');

# Finished
sleep (1);              # Give 0MQ time to deliver
