# Hello World server in Perl

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_REP);

# Socket to talk to clients
my $context = ZMQ::FFI->new();
my $responder = $context->socket(ZMQ_REP);
$responder->bind("tcp://*:5555");

while (1) {
    $responder->recv();
    say "Received Hello";
    sleep 1;
    $responder->send("World");
}
