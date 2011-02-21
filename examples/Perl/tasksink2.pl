#!/usr/bin/perl
=pod

Task sink - design 2
Adds pub-sub flow to send kill signal to workers

Based on examples/C/tasksink2.c; translated to Perl by darksuji

=cut

use strict;
use warnings;
use feature ':5.10';

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

# Send kill signal to workers
$controller->send('KILL');

# Finished
sleep (1);              # Give 0MQ time to deliver
