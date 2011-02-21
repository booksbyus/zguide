#!/usr/bin/perl
=pod

Simple message queuing broker

Same as request-reply broker but using QUEUE device

Based on examples/C/msgqueue.c; translated to Perl by darksuji

=cut

use strict;
use warnings;
use feature ':5.10';

use ZeroMQ qw/:all/;
use ZeroMQ::Raw qw/zmq_device/;

my $context = ZeroMQ::Context->new();

# Socket facing clients
my $frontend = $context->socket(ZMQ_XREP);
$frontend->bind('tcp://*:5559');

# Socket facing services
my $backend = $context->socket(ZMQ_XREQ);
$backend->bind('tcp://*:5560');

# Start built-in device
zmq_device(ZMQ_QUEUE, $frontend->socket, $backend->socket); # FIXME:  Should use higher-level interface once one exists...

# We never get here...
