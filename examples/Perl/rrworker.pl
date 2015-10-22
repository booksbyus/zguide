# Hello world worker in Perl
# Connects REP socket to tcp://localhost:5560
# Expects "Hello from client, replies with "World"

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_REP);

my $context = ZMQ::FFI->new();

# Socket to talk to clients
my $responder = $context->socket(ZMQ_REP);
$responder->connect('tcp://localhost:5560');

while (1) {
    # Wait for next request from client
    my $string = $responder->recv();
    say "Received request: [$string]";

    # Do some 'work'
    sleep 1;

    # Send reply back to client
    $responder->send("World");
}
