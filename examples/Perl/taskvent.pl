#!/usr/bin/perl
=pod

Task ventilator

Binds PUSH socket to tcp://localhost:5557

Sends batch of tasks to workers via that socket

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZeroMQ qw/:all/;

sub within {
    my ($upper) = @_;

    return int(rand($upper)) + 1;
}

my $context = ZeroMQ::Context->new();

#  Socket to send messages on
my $sender = $context->socket(ZMQ_PUSH);
$sender->bind('tcp://*:5557');

print 'Press Enter when the workers are ready: ';
<STDIN>;
say 'Sending tasks to workers...';

# The first message is "0" and signals start of batch
$sender->send('0');

# Send 100 tasks
my $total_msec = 0;     # Total expected cost in msecs
for (1 .. 100) {
    # Random workload from 1 to 100msecs
    my $workload = within(100);
    $total_msec += $workload;
    $sender->send($workload);
}
say "Total expected cost: $total_msec msec";
sleep (1);              # Give 0MQ time to deliver
