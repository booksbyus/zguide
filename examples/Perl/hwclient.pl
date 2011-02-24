#!/usr/bin/perl
=pod

Hello World client

Connects REQ socket to tcp://localhost:5555

Sends "Hello" to server, expects "World" back

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZeroMQ qw/:all/;

my $context = ZeroMQ::Context->new();

# Socket to talk to server
say 'Connecting to hello world server...';
my $requester = $context->socket(ZMQ_REQ);
$requester->connect('tcp://localhost:5555');

for my $request_nbr (0..9) {
    say "Sending request $request_nbr...";
    $requester->send('Hello');

    my $reply = $requester->recv();
    say "Received reply $request_nbr: [". $reply->data .']';
}
