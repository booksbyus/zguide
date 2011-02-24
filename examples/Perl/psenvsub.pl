#!/usr/bin/perl
=pod

Pubsub envelope subscriber

Based on examples/C/psenvsub.c; translated to Perl by darksuji

=cut

use strict;
use warnings;
use 5.10.0;

use ZeroMQ qw/:all/;

# Prepare our context and subscriber
my $context = ZeroMQ::Context->new();
my $subscriber = $context->socket(ZMQ_SUB);
$subscriber->connect('tcp://localhost:5563');
$subscriber->setsockopt(ZMQ_SUBSCRIBE, 'B');

while (1) {
    # Read envelope with address
    my $address = $subscriber->recv()->data;
    # Read message contents
    my $contents = $subscriber->recv()->data;
    printf("[%s] %s\n", $address, $contents);
}
