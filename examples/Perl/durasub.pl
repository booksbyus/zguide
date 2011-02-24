#!/usr/bin/perl
=pod

Durable subscriber

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZeroMQ qw/:all/;

my $context = ZeroMQ::Context->new();

# Connect our subscriber socket
my $subscriber = $context->socket(ZMQ_SUB);
$subscriber->setsockopt(ZMQ_IDENTITY, 'Hello');
$subscriber->setsockopt(ZMQ_SUBSCRIBE, '');
$subscriber->connect('tcp://localhost:5565');

# Synchronize with publisher
my $sync = $context->socket(ZMQ_PUSH);
$sync->connect('tcp://localhost:5564');
$sync->send('');

# Get updates, expect random Ctrl-C death
while (1) {
    my $string = $subscriber->recv()->data;
    say $string;
    last if $string eq 'END';
}
