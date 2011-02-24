#!/usr/bin/perl
=pod

Weather proxy device

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZeroMQ qw/:all/;

my $context = ZeroMQ::Context->new();

# This is where the weather server sits
my $frontend = $context->socket(ZMQ_SUB);
$frontend->connect('tcp://192.168.55.210:5556');

# This is our public endpoint for subscribers
my $backend = $context->socket(ZMQ_PUB);
$backend->bind('tcp://10.1.1.0:8100');

# Subscribe on everything
$frontend->setsockopt(ZMQ_SUBSCRIBE, '');

# Shunt messages out to our own subscribers
while (1) {
    while (1) {
        # Process all parts of the message
        my $message = $frontend->recv();
        my $more = $frontend->getsockopt(ZMQ_RCVMORE);
        $backend->send($message, $more ? ZMQ_SNDMORE : 0);
        last unless $more;
    }
}
