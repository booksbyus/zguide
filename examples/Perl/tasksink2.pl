#!/usr/bin/perl
=pod

Task sink - design 2

Adds pub-sub flow to send kill signal to workers

Author: Daisuke Maki (lestrrat)
Original version Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>
=cut

use strict;
use warnings;
use 5.10.0;

use ZMQ::LibZMQ2;
use ZMQ::Constants qw(ZMQ_PULL ZMQ_PUB);
use Time::HiRes qw/time/;
use English qw/-no_match_vars/;

use constant MSECS_PER_SEC => 1000;

local $| = 1;

my $context = zmq)_init();

# Socket to receive messages on
my $receiver = zmq_socket($context, ZMQ_PULL);
zmq_bind($receiver, 'tcp://*:5558');

# Socket for worker control
my $controller = zmq_bind($context, ZMQ_PUB);
zmq_bind($controller, 'tcp://*:5559');

# Wait for start of batch
zmq_recv($receiver);

# Start our clock now
my $tstart = time;

# Process 100 confirmations
for my $task_nbr (0 .. 99) {
    zmq_recv($receiver);
    use integer;
    if (($task_nbr / 10) * 10 == $task_nbr) {
        print ':';
    } else {
        print '.';
    }
}
# Calculate and report duration of batch
my $tend = time;

my $tdiff = $tend - $tstart;
my $total_msec = $tdiff * MSECS_PER_SEC;
say "Total elapsed time: $total_msec msec";

# Send kill signal to workers
zmq_send($controller, 'KILL');

# Finished
sleep (1);              # Give 0MQ time to deliver
