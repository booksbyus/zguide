#!/usr/bin/perl
=pod

Simple request-reply broker

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use ZeroMQ qw/:all/;

# Prepare our context and sockets
my $context = ZeroMQ::Context->new();
my $frontend = $context->socket(ZMQ_ROUTER);
my $backend  = $context->socket(ZMQ_DEALER);
$frontend->bind('tcp://*:5559');
$backend->bind('tcp://*:5560');

# Initialize poll set
my $poller = ZeroMQ::Poller->new(
    {
        name    => 'frontend',
        socket  => $frontend,
        events  => ZMQ_POLLIN,
    }, {
        name    => 'backend',
        socket  => $backend,
        events  => ZMQ_POLLIN,
    },
);

# Switch messages between sockets
while (1) {
    $poller->poll();
    if ($poller->has_event('frontend')) {
        while (1) {
            # Process all parts of the message
            my $message = $frontend->recv();
            my $more = $frontend->getsockopt(ZMQ_RCVMORE);
            $backend->send($message, $more ? ZMQ_SNDMORE : 0);
            last unless $more;
        }
    }
    if ($poller->has_event('backend')) {
        while (1) {
            # Process all parts of the message
            my $message = $backend->recv();
            my $more = $backend->getsockopt(ZMQ_RCVMORE);
            $frontend->send($message, $more ? ZMQ_SNDMORE : 0);
            last unless $more;
        }
    }
}
