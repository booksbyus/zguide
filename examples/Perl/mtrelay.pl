#!/usr/bin/perl
=pod

Multithreaded relay

NOTE:  As of v0.09, ZeroMQ does not allow us to pass sockets around so as to
retain compatibility with ligzmq-2.0.  This example is therefore not a precise
rendition of the official, libzmq-2.1-exploiting C example.

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;
use threads;

use ZeroMQ qw/:all/;

sub step1 {
    my ($context) = @_;

    my $socket = $context->socket(ZMQ_PAIR);
    $socket->connect('inproc://step2');

    # Signal downstream to step 2
    $socket->send('');
    return;
}

sub step2 {
    my ($context) = @_;

    my $socket = $context->socket(ZMQ_PAIR);
    $socket->connect('inproc://step3');

    my $receiver = $context->socket(ZMQ_PAIR);
    $receiver->bind('inproc://step2');
    threads->create('step1', $context)->detach();

    # Wait for signal
    $receiver->recv();

    # Signal downstream to step 3
    $socket->send('');
    return;
}

my $context = ZeroMQ::Context->new();

my $receiver = $context->socket(ZMQ_PAIR);
$receiver->bind('inproc://step3');
threads->create('step2', $context)->detach();

# Wait for signal
$receiver->recv();

say 'Test successful!';
