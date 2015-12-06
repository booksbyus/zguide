# Synchronized subscriber in Perl

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_SUB ZMQ_REQ ZMQ_RCVHWM);

my $context = ZMQ::FFI->new();

# First, connect our subscriber socket
my $subscriber = $context->socket(ZMQ_SUB);
$subscriber->set(ZMQ_RCVHWM, 'int', 0);
$subscriber->connect('tcp://localhost:5561');
$subscriber->subscribe('');

# 0MQ is so fast, we need to wait a while...
sleep 3;

# Second, synchronize with publisher
my $syncclient = $context->socket(ZMQ_REQ);
$syncclient->connect('tcp://localhost:5562');

# send a synchronization request
$syncclient->send('');

# wait for synchronization reply
$syncclient->recv();

# Third, get our updates and report how many we got
my $update_nbr = 0;
while (1) {
    last if $subscriber->recv() eq "END";
    $update_nbr++;
}

say "Received $update_nbr updates";
