# Pubsub envelope subscriber in Perl

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_SUB);

# Prepare our context and subscriber
my $context    = ZMQ::FFI->new();
my $subscriber = $context->socket(ZMQ_SUB);
$subscriber->connect('tcp://localhost:5563');
$subscriber->subscribe('B');

while (1) {
    # Read envelope with address
    my ($address, $contents) = $subscriber->recv_multipart();

    say "[$address] $contents";
}

# We never get here
