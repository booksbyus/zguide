#!/usr/bin/perl
=pod

Publisher for durable subscriber

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZeroMQ qw/:all/;

my $context = ZeroMQ::Context->new();

# Subscriber tells us when it's ready here
my $sync = $context->socket(ZMQ_PULL);
$sync->bind('tcp://*:5564');

# We send updates via this socket
my $publisher = $context->socket(ZMQ_PUB);
$publisher->bind('tcp://*:5565');

# Prevent publisher overflow from slow subscribers
$publisher->setsockopt(ZMQ_HWM, 1);

# Specify swap space in bytes, this covers all subscribers
$publisher->setsockopt(ZMQ_SWAP, 25_000_000);

# Wait for synchronization request
$sync->recv();

# Now broadcast exactly 10 updates with pause
for my $update_nbr (0 .. 9) {
    $publisher->send("Update $update_nbr");
    sleep (1);
}
$publisher->send('END');
