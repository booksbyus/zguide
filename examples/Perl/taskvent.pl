# Task ventilator
# Binds PUSH socket to tcp://localhost:5557
# Sends batch of tasks to workers via that socket

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_PUSH);

my $ctx = ZMQ::FFI->new();

# Socket to send messages on
my $sender = $ctx->socket(ZMQ_PUSH);
$sender->bind('tcp://*:5557');

# Socket to send start of batch message on
my $sink = $ctx->socket(ZMQ_PUSH);
$sink->connect('tcp://localhost:5558');

say "Press Enter when the workers are ready: ";
<STDIN>;
say "Sending tasks to workers...";

# The first message is "0" and signals start of batch
$sink->send('0');

# Send 100 tasks
my $total_msec = 0;
my $workload;
for (1..100) {
    $workload    = int(rand(100) + 1);
    $total_msec += $workload;

    $sender->send($workload);
}

say "Total expected cost: $total_msec msec";
