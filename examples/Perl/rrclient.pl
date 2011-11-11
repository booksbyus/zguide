#!/usr/bin/perl
=pod

Hello World client

Connects REQ socket to tcp://localhost:5559

Sends "Hello" to server, expects "World" back

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZeroMQ qw/:all/;

my $context = ZeroMQ::Context->new();

# Socket to talk to server
my $requester = $context->socket(ZMQ_REQ);
$requester->connect('tcp://localhost:5559');

for my $request_nbr (0 .. 9) {
    $requester->send('Hello');
    my $string = $requester->recv()->data;
    say "Received reply $request_nbr [$string]";
}
