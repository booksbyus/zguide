# Hello world client in Perl
# Connects REQ socket to tcp://localhost:5559
# Sends "Hello" to server, expects "World" back

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_REQ);

my $context = ZMQ::FFI->new();

# Socket to talk to server
my $requester = $context->socket(ZMQ_REQ);
$requester->connect('tcp://localhost:5559');

for my $request_nbr (1..10) {
    $requester->send("Hello");
    my $string = $requester->recv();
    say "Received reply $request_nbr [$string]";
}
