#!/usr/bin/perl
=pod

Multithreaded relay

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;
use threads;

use ZMQ::LibZMQ3;
use ZMQ::Constants qw(ZMQ_PAIR);
use zhelpers;

sub step1 {
    my ($context) = @_;

    # Connect to step2 and tell it we're ready
    my $xmitter = zmq_socket($context, ZMQ_PAIR);
    zmq_connect($xmitter, 'inproc://step2');
    say 'Step 1 ready, signaling step 2';
    s_send($xmitter, '');

    return;
}

sub step2 {
    my ($context) = @_;

    # Bind inproc socket before starting step1
    my $receiver = zmq_socket($context, ZMQ_PAIR);
    zmq_bind($receiver, 'inproc://step2');
    threads->create('step1', $context)->detach();

    # Wait for signal
    s_recv($receiver);

    # Connect to step3 and tell it we're ready
    my $xmitter = zmq_socket($context, ZMQ_PAIR);
    zmq_connect($xmitter, 'inproc://step3');
    say 'Step 2 ready, signaling step 3';
    s_send($xmitter, '');

    return;
}

my $context = zmq_init();

my $receiver = zmq_socket($context, ZMQ_PAIR);
zmq_bind($receiver, 'inproc://step3');
threads->create('step2', $context)->detach();

# Wait for signal
s_recv($receiver);

say 'Test successful!';
