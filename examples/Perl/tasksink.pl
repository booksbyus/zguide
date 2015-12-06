# Task sink in Perl
# Binds PULL socket to tcp://localhost:5558
# Collects results from workers via that socket

use strict;
use warnings;
use v5.10;

use Time::HiRes qw(time);

$| = 1; # autoflush stdout after each print

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_PULL);

# Prepare our context and socket
my $ctx      = ZMQ::FFI->new();
my $receiver = $ctx->socket(ZMQ_PULL);
$receiver->bind('tcp://*:5558');

# Wait for start of batch
my $string = $receiver->recv();

# Start our clock now;
my $start_time = time();

# Process 100 confirmations
for my $task_nbr (0..99) {
    $receiver->recv();

    if ($task_nbr % 10 == 0) {
        print ":";
    }
    else {
        print ".";
    }
}

# Calculate and report duration of batch
printf "Total elapsed time: %d msec\n",
    (time() - $start_time) * 1000;
