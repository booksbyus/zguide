#!/usr/bin/perl
=pod

Synchronized publisher

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZeroMQ qw/:all/;

# We wait for 10 subscribers
use constant SUBSCRIBERS_EXPECTED => 10;

my $context = ZeroMQ::Context->new();

# Socket to talk to clients
my $publisher = $context->socket(ZMQ_PUB);
$publisher->bind('tcp://*:5561');

# Socket to receive signals
my $syncservice = $context->socket(ZMQ_REP);
$syncservice->bind('tcp://*:5562');

# Get synchronization from subscribers
for (1 .. SUBSCRIBERS_EXPECTED) {
    # - wait for synchronization request
    $syncservice->recv();
    # - send synchronization reply
    $syncservice->send('');
}
# Now broadcast exactly 1M updates followed by END
for (1 .. 1_000_000) {
    $publisher->send('Rhubarb');
}
$publisher->send('END');
