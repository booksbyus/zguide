# Multithreaded Hello World server in Perl

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_REP ZMQ_ROUTER ZMQ_DEALER);

use threads;

sub worker_routine {
    my ($context) = @_;

    # Socket to talk to dispatcher
    my $receiver = $context->socket(ZMQ_REP);
    $receiver->connect('inproc://workers');

    while (1) {
        my $string = $receiver->recv();

        say "Received request: [$string]";

        # Do some 'work'
        sleep 1;

        # Send reply back to client
        $receiver->send('World');
    }
}

my $context = ZMQ::FFI->new();

# Socket to talk to clients
my $clients = $context->socket(ZMQ_ROUTER);
$clients->bind('tcp://*:5555');

# Socket to talk to workers
my $workers = $context->socket(ZMQ_DEALER);
$workers->bind('inproc://workers');

# Launch pool of worker threads
for (1..5) {
    threads->create('worker_routine', $context);
}

# Connect work threads to client threads via a queue proxy
$context->proxy($clients, $workers);

# We never get here
