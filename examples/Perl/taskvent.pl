#!/usr/bin/perl
=pod

Task ventilator

Binds PUSH socket to tcp://localhost:5557

Sends batch of tasks to workers via that socket

Author: Daisuke Maki (lestrrat)
Original Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZMQ::LibZMQ2;
use ZMQ::Constants qw(ZMQ_PUSH);

sub within {
    my ($upper) = @_;

    return int(rand($upper)) + 1;
}

my $context = zmq_init();

#  Socket to send messages on
my $sender = zmq_socket($context, ZMQ_PUSH);
zmq_bind($sender, 'tcp://*:5557');

print 'Press Enter when the workers are ready: ';
<STDIN>;
say 'Sending tasks to workers...';

# The first message is "0" and signals start of batch
zmq_send($sender, '0');

# Send 100 tasks
my $total_msec = 0;     # Total expected cost in msecs
for (1 .. 100) {
    # Random workload from 1 to 100msecs
    my $workload = within(100);
    $total_msec += $workload;
    zmq_send($sender, $workload);
}
say "Total expected cost: $total_msec msec";
sleep (1);              # Give 0MQ time to deliver
