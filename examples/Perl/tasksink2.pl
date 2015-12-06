# Task sink - design 2 in Perl
# Adds pub-sub flow to send kill signal to workers

use strict;
use warnings;
use v5.10;

use Time::HiRes qw(time);

$| = 1; # autoflush stdout after each print

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_PULL ZMQ_PUB);

# Socket to receive messages on
my $context = ZMQ::FFI->new();
my $receiver = $context->socket(ZMQ_PULL);
$receiver->bind('tcp://*:5558');

# Socket for worker control
my $controller = $context->socket(ZMQ_PUB);
$controller->bind('tcp://*:5559');

# Wait for start of batch
my $string = $receiver->recv();

# Start our clock now
my $start_time = time();

# Process 100 confirmations
for my $task_nbr (1..100) {
    $receiver->recv();

    if ( ($task_nbr % 10) == 0 ) {
        print ":";
    }
    else {
        print ".";
    }
}

# Calculate and report duration of batch
printf "Total elapsed time: %d msec\n",
    (time() - $start_time) * 1000;

# Send kill signal to workers
$controller->send("KILL");
