#!/usr/bin/perl
=pod

Multithreaded Hello World server

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;
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
        say "Received request: [$string]");
        # Do some 'work'
        sleep (1);
        # Send reply back to client
        $receiver->send('World');
    }
}

# Prepare our context and sockets
my $context = ZeroMQ::Context->new();

# Socket to talk to clients
my $clients = $context->socket(ZMQ_ROUTER);
$clients->bind('tcp://*:5555');

# Socket to talk to workers
my $workers = $context->socket(ZMQ_DEALER);
$workers->bind('inproc://workers');

# Launch pool of worker threads
for (1 .. 5) {
    threads->create('worker_routine', $context);
}
# Connect work threads to client threads via a queue
# Having to send raw sockets here is an infelicity in ZeroMQ 0.09
zmq_device (ZMQ_QUEUE, $clients->socket, $workers->socket);

# We never get here but clean up anyhow
