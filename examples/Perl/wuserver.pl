# Weather update server in Perl
# Binds PUB socket to tcp://*:5556
# Publishes random weather updates

use strict;
use warnings;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_PUB);

my $context   = ZMQ::FFI->new();
my $publisher = $context->socket(ZMQ_PUB);
$publisher->bind("tcp://*:5556");

my ($update, $zipcode, $temperature, $relhumidity);

while (1) {
    $zipcode     = rand(100_000);
    $temperature = rand(215) - 80;
    $relhumidity = rand(50) + 10;

    $update = sprintf('%d %d %d', $zipcode,$temperature,$relhumidity);

    $publisher->send($update);
}
