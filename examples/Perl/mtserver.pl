#!/usr/bin/perl
=pod

Multithreaded Hello World server

Author: Daisuke Maki (lestrrat)
Original version Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;
use threads;

use ZMQ::LibZMQ2;
use ZMQ::Constants qw(ZMQ_REP ZMQ_ROUTER ZMQ_DEALER ZMQ_QUEUE);

sub worker_routine {
    my ($context) = @_;

    # Socket to talk to dispatcher
    my $receiver = zmq_socket($context, ZMQ_REP);
    zmq_connect($receiver, 'inproc://workers');

    while (1) {
        my $string = zmq_msg_data(zmq_recv($receiver));
        say "Received request: [$string]";
        # Do some 'work'
        sleep (1);
        # Send reply back to client
        zmq_send($receiver, 'World');
    }
}

# Prepare our context and sockets
my $context = zmq_init();

# Socket to talk to clients
my $clients = zmq_socket($context, ZMQ_ROUTER);
zmq_bind($clients, 'tcp://*:5555');

# Socket to talk to workers
my $workers = zmq_socket($context, ZMQ_DEALER);
zmq_bind($workers, 'inproc://workers');

# Launch pool of worker threads
for (1 .. 5) {
    threads->create('worker_routine', $context);
}
zmq_device (ZMQ_QUEUE, $clients, $workers);

# We never get here but clean up anyhow
