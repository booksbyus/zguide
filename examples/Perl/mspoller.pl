#!/usr/bin/perl
=pod

Reading from multiple sockets

This version uses zmq_poll()

Based on examples/C/mspoller.c; translated to Perl by darksuji

=cut

use strict;
use warnings;
use feature ':5.10';

use ZeroMQ qw/:all/;

my $context = ZeroMQ::Context->new();

# Connect to task ventilator
my $receiver = $context->socket(ZMQ_PULL);
$receiver->connect('tcp://localhost:5557');

# Connect to weather server
my $subscriber = $context->socket(ZMQ_SUB);
$subscriber->connect('tcp://localhost:5556');
$subscriber->setsockopt(ZMQ_SUBSCRIBE, '10001 ');

# Initialize poll set
my $poller = ZeroMQ::Poller->new(
    {
        name    => 'receiver',
        socket  => $receiver,
        events  => ZMQ_POLLIN,
    }, {
        name    => 'subscriber',
        socket  => $subscriber,
        events  => ZMQ_POLLIN,
    },
);
# Process messages from both sockets
while (1) {
    $poller->poll();
    if ($poller->has_event('receiver')) {
        my $message = $receiver->recv();
        # Process task
    }
    if ($poller->has_event('subscriber')) {
        my $message = $subscriber->recv();
        # Process weather update
    }
}
# We never get here
