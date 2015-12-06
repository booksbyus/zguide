# Task worker in Perl
# Connects PULL socket to tcp://localhost:5557
# Collects workloads from ventilator via that socket
# Connects PUSH socket to tcp://localhost:5558
# Sends results to sink via that socket

use strict;
use warnings;

$| = 1; # autoflush stdout after each print

use Time::HiRes qw(usleep);

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_PUSH ZMQ_PULL);

my $context = ZMQ::FFI->new();

# Socket to receive messages on
my $receiver = $context->socket(ZMQ_PULL);
$receiver->connect('tcp://localhost:5557');

# Socket to send messages on
my $sender = $context->socket(ZMQ_PUSH);
$sender->connect('tcp://localhost:5558');

# Process tasks forever
my $string;
while (1) {
    $string = $receiver->recv();

    print "$string.";    # Show progress
    usleep $string*1000; # Do the work
    $sender->send("");   # Send results to sink
}
