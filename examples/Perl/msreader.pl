# Reading from multiple sockets in Perl
# This version uses a simple recv loop

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_PULL ZMQ_SUB ZMQ_DONTWAIT);

use TryCatch;
use Time::HiRes qw(usleep);

# Connect to task ventilator
my $context = ZMQ::FFI->new();
my $receiver = $context->socket(ZMQ_PULL);
$receiver->connect('tcp://localhost:5557');

# Connect to weather server
my $subscriber = $context->socket(ZMQ_SUB);
$subscriber->connect('tcp://localhost:5556');
$subscriber->subscribe('10001');

# Process messages from both sockets
# We prioritize traffic from the task ventilator
while (1) {

    PROCESS_TASK:
    while (1) {
        try {
            my $msg = $receiver->recv(ZMQ_DONTWAIT);
            # Process task
        }
        catch {
            last PROCESS_TASK;
        }
    }

    PROCESS_UPDATE:
    while (1) {
        try {
            my $msg = $subscriber->recv(ZMQ_DONTWAIT);
            # Process weather update
        }
        catch {
            last PROCESS_UPDATE;
        }
    }

    # No activity, so sleep for 1 msec
    usleep(1000);
}
