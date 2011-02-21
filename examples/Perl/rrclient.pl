#!/usr/bin/perl
=pod

Hello World client

Connects REQ socket to tcp://localhost:5559

Sends "Hello" to server, expects "World" back

Based on examples/C/rrclient.c; translated to Perl by darksuji

=cut

use strict;
use warnings;
use feature ':5.10';

use ZeroMQ qw/:all/;

my $context = ZeroMQ::Context->new();

# Socket to talk to server
my $requester = $context->socket(ZMQ_REQ);
$requester->connect('tcp://localhost:5559');

for (my $request_count = 0; $request_count < 10; ++$request_count) {
    $requester->send('Hello');
    my $string = $requester->recv()->data;
    printf ("Received reply %d [%s]\n", $request_count, $string);
}
