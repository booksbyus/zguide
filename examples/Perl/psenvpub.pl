# Pubsub envelope publisher in Perl

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_PUB);

# Prepare our context and publisher
my $context   = ZMQ::FFI->new();
my $publisher = $context->socket(ZMQ_PUB);
$publisher->bind('tcp://*:5563');

while (1) {
    # Write two messages, each with an envelope and content
    $publisher->send_multipart(["A", "We don't want to see this"]);
    $publisher->send_multipart(["B", "We would like to see this"]);

    sleep 1;
}

# We never get here
