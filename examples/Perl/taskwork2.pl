#!/usr/bin/perl
=pod

Task worker - design 2

Adds pub-sub flow to receive and respond to kill signal

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

use IO::Handle;

use ZeroMQ qw/:all/;
use Time::HiRes qw/nanosleep/;

use constant NSECS_PER_MSEC => 1_000_000;

my $context = ZeroMQ::Context->new();

# Socket to receive messages on
my $receiver = $context->socket(ZMQ_PULL);
$receiver->connect('tcp://localhost:5557');

# Socket to send messages to
my $sender = $context->socket(ZMQ_PUSH);
$sender->connect('tcp://localhost:5558');

# Socket for control input
my $controller = $context->socket(ZMQ_SUB);
$controller->connect('tcp://localhost:5559');
$controller->setsockopt(ZMQ_SUBSCRIBE, '');

# Process messages from receiver and controller
my $poller = ZeroMQ::Poller->new(
    {
        name    => 'receiver',
        socket  => $receiver,
        events  => ZMQ_POLLIN,
    }, {
        name    => 'controller',
        socket  => $controller,
        events  => ZMQ_POLLIN,
    },
);

# Process messages from both sockets
while (1) {
    $poller->poll();
    if ( $poller->has_event('receiver') ) {
        my $message = $receiver->recv();

        # Process task
        my $workload = $message->data * NSECS_PER_MSEC;

        # Do the work
        nanosleep $workload;

        # Send results to sink
        $sender->send('');

        # Simple progress indicator for the viewer
        STDOUT->printflush('.');
    }
    # Any waiting controller command acts as 'KILL'
    last if $poller->has_event('controller');
}
# Finished
