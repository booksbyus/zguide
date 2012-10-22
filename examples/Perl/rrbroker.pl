#!/usr/bin/perl
=pod

Simple request-reply broker

Author: Daisuke Maki (lestrrat)
Original version Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZMQ::LibZMQ2;
use ZMQ::Constants qw(ZMQ_ROUTER ZMQ_DEALER ZMQ_POLLIN ZMQ_RCVMORE ZMQ_SNDMORE);

# Prepare our context and sockets
my $context = zmq_init();
my $frontend = zmq_socket($context, ZMQ_ROUTER);
my $backend  = zmq_socket($context, ZMQ_DEALER);
zmq_bind($frontend, 'tcp://*:5559');
zmq_bind($backend, 'tcp://*:5560');

# Initialize poll set
my @poll = (
    {
        socket  => $frontend,
        events  => ZMQ_POLLIN,
        callback => sub {
            while (1) {
                # Process all parts of the message
                my $message = zmq_recv($frontend);
                my $more = zmq_getsockopt($frontend, ZMQ_RCVMORE);
                zmq_send($backend, $message, $more ? ZMQ_SNDMORE : 0);
                last unless $more;
            }
        }
    }, {
        socket  => $backend,
        events  => ZMQ_POLLIN,
        callback => sub {
            while (1) {
                # Process all parts of the message
                my $message = zmq_recv($backend);
                my $more = zmq_getsockopt($backend, ZMQ_RCVMORE);
                zmq_send($frontend, $message, $more ? ZMQ_SNDMORE : 0);
                last unless $more;
            }
        }
    },
);

# Switch messages between sockets
while (1) {
    zmq_poll(\@poll);
}
