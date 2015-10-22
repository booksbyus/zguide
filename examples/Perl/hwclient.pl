# Hello World client in Perl

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_REQ);

say "Connecting to hello world server...";
my $context = ZMQ::FFI->new();
my $requestor = $context->socket(ZMQ_REQ);
$requestor->connect("tcp://localhost:5555");

for my $request_nbr (0..9) {
    say "Sending Hello $request_nbr...";
    $requestor->send("Hello");
    $requestor->recv();
    say "Received World $request_nbr";
}
