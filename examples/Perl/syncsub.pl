#!/usr/bin/perl
=pod

Synchronized subscriber

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZeroMQ qw/:all/;

my $context = ZeroMQ::Context->new();

# First, connect our subscriber socket
my $subscriber = $context->socket(ZMQ_SUB);
$subscriber->connect('tcp://localhost:5561');
$subscriber->setsockopt(ZMQ_SUBSCRIBE, '');

# 0MQ is so fast, we need to wait a while...
sleep (1);

# Second, synchronize with publisher
my $syncclient = $context->socket(ZMQ_REQ);
$syncclient->connect('tcp://localhost:5562');

# - send a synchronization request
$syncclient->send('');

# - wait for synchronization reply
$syncclient->recv();

# Third, get our updates and report how many we got
my $update_nbr = 0;
while (1) {
    my $string = $subscriber->recv()->data;
    last if $string eq 'END';
    $update_nbr++;
}
say "Received $update_nbr updates";
