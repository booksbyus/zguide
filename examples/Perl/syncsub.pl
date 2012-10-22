#!/usr/bin/perl
=pod

Synchronized subscriber

Author: Daisuke Maki (lestrrat)
Original version Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZMQ::LibZMQ2;
use ZMQ::Constants qw(ZMQ_SUB ZMQ_SUBSCRIBE ZMQ_REQ);

my $context = zmq_init();

# First, connect our subscriber socket
my $subscriber = zmq_socket($context, ZMQ_SUB);
zmq_connect($subscriber, 'tcp://localhost:5561');
zmq_setsockopt($subscriber, ZMQ_SUBSCRIBE, '');

# 0MQ is so fast, we need to wait a while...
sleep (1);

# Second, synchronize with publisher
my $syncclient = zmq_socket($context, ZMQ_REQ);
zmq_connect($syncclient, 'tcp://localhost:5562');

# - send a synchronization request
zmq_send($syncclient, '');

# - wait for synchronization reply
zmq_recv($syncclient);

# Third, get our updates and report how many we got
my $update_nbr = 0;
while (1) {
    my $string = zmq_msg_data(zmq_recv($subscriber));
    last if $string eq 'END';
    $update_nbr++;
}
say "Received $update_nbr updates";
