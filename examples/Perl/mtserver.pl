#!/usr/bin/perl
=pod

Multithreaded Hello World server

Based on examples/C/mtserver.c; translated to Perl by darksuji

=cut

use strict;
use warnings;
use feature ':5.10';
use threads;

use ZeroMQ qw/:all/;
use ZeroMQ::Raw qw/zmq_device/;

sub worker_routine {
    my ($context) = @_;

    # Socket to talk to dispatcher
    my $receiver = $context->socket(ZMQ_REP);
    $receiver->connect('inproc://workers');

    while (1) {
        my $string = $receiver->recv()->data;
        printf("Received request: [%s]\n", $string);
        # Do some 'work'
        sleep (1);
        # Send reply back to client
        $receiver->send('World');
    }
}

# Prepare our context and sockets
my $context = ZeroMQ::Context->new();

# Socket to talk to clients
my $clients = $context->socket(ZMQ_XREP);
$clients->bind('tcp://*:5555');

# Socket to talk to workers
my $workers = $context->socket(ZMQ_XREQ);
$workers->bind('inproc://workers');

# Launch pool of worker threads
for (1 .. 5) {
    threads->create('worker_routine', $context);
}
# Connect work threads to client threads via a queue
zmq_device (ZMQ_QUEUE, $clients->socket, $workers->socket); # FIXME:  Higher-level abstraction please...

# We never get here but clean up anyhow
