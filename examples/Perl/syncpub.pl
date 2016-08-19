# Synchronized publisher in Perl

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_PUB ZMQ_REP ZMQ_SNDHWM);

my $SUBSCRIBERS_EXPECTED = 10; # We wait for 10 subscribers

my $context = ZMQ::FFI->new();

# Socket to talk to clients
my $publisher = $context->socket(ZMQ_PUB);
$publisher->set(ZMQ_SNDHWM, 'int', 0);
$publisher->set_linger(-1);
$publisher->bind('tcp://*:5561');

# Socket to receive signals
my $syncservice = $context->socket(ZMQ_REP);
$syncservice->bind('tcp://*:5562');

# Get synchronization from subscribers
say "Waiting for subscribers";

for my $subscribers (1..$SUBSCRIBERS_EXPECTED) {
    # wait for synchronization request
    $syncservice->recv();

    # send synchronization reply
    $syncservice->send('');

    say "+1 subscriber ($subscribers/$SUBSCRIBERS_EXPECTED)";
}

# Now broadcast exactly 1M updates followed by END
say "Broadcasting messages";

for (1..1_000_000) {
    $publisher->send("Rhubarb");
}

$publisher->send("END");
say "Done";
