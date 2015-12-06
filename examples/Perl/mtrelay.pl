# Multithreaded relay in Perl

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_PAIR);

use threads;

sub step1 {
    my ($context) = @_;

    # Connect to step2 and tell it we're ready
    my $xmitter = $context->socket(ZMQ_PAIR);
    $xmitter->connect('inproc://step2');

    say "Step 1 ready, signaling step 2";

    $xmitter->send("READY");
}

sub step2 {
    my ($context) = @_;

    # Bind inproc socket before starting step1
    my $receiver = $context->socket(ZMQ_PAIR);
    $receiver->bind('inproc://step2');

    threads->create('step1', $context)
           ->detach();

    # Wait for signal and pass it on
    my $string = $receiver->recv();

    # Connect to step3 and tell it we're ready
    my $xmitter = $context->socket(ZMQ_PAIR);
    $xmitter->connect('inproc://step3');

    say "Step 2 ready, signaling step 3";
    $xmitter->send("READY");
}

my $context = ZMQ::FFI->new();

# Bind inproc socket before starting step2
my $receiver = $context->socket(ZMQ_PAIR);
$receiver->bind('inproc://step3');

threads->create('step2', $context)
       ->detach();

# Wait for signal
$receiver->recv();

say "Test successful!";
