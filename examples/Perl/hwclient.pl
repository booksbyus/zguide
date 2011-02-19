#!/usr/bin/perl
=pod

Hello World client

Connects REQ socket to tcp://localhost:5555

Sends "Hello" to server, expects "World" back

Based on examples/C/hwclient.c; translated to Perl by darksuji

=cut

use strict;
use warnings;
use feature ':5.12';

use ZeroMQ qw/:all/;

my $context = ZeroMQ::Context->new();

# Socket to talk to server
say 'Connecting to hello world server...';
my $requester = $context->socket(ZMQ_REQ);
$requester->connect('tcp://localhost:5555');

for my $request_number (0..9) {
    say "Sending request $request_number...";
    $requester->send('Hello');

    my $reply = $requester->recv();
    say "Received reply $request_number: [". $reply->data .']';
}
