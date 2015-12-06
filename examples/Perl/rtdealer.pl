# ROUTER-to-DEALER in Perl

use strict;
use warnings;
use v5.10;

use threads;
use Time::HiRes qw(usleep);

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_DEALER ZMQ_ROUTER);

my $NBR_WORKERS = 10;

sub worker_task {

    my $context = ZMQ::FFI->new();
    my $worker = $context->socket(ZMQ_DEALER);

    $worker->set_identity(Time::HiRes::time());
    $worker->connect('tcp://localhost:5671');

    my $total = 0;
    WORKER_LOOP:
    while (1) {
        # Tell the broker we're ready for work
        $worker->send_multipart(['', 'Hi Boss']);

        # Get workload from broker, until finished
        my ($delimiter, $workload) = $worker->recv_multipart();
        my $finished = $workload eq "Fired!";
        if ($finished) {
            say "Completed $total tasks";
            last WORKER_LOOP;
        }
        $total++;

        # Do some random work
        usleep int(rand(500_000)) + 1;
    }
}

# While this example runs in a single process, that is only to make
# it easier to start and stop the example. Each thread has its own
# context and conceptually acts as a separate process.

my $context = ZMQ::FFI->new();
my $broker = $context->socket(ZMQ_ROUTER);

$broker->bind('tcp://*:5671');

for my $worker_nbr (1..$NBR_WORKERS) {
    threads->create('worker_task')->detach();
}

# Run for five seconds and then tell workers to end
my $end_time = time() + 5;
my $workers_fired = 0;

BROKER_LOOP:
while (1) {
    # Next message gives us least recently used worker
    my ($identity, $delimiter, $response) = $broker->recv_multipart();

    # Encourage workers until it's time to fire them
    if ( time() < $end_time ) {
        $broker->send_multipart([$identity, '', 'Work harder']);
    }
    else {
        $broker->send_multipart([$identity, '', 'Fired!']);
        if ( ++$workers_fired == $NBR_WORKERS) {
            last BROKER_LOOP;
        }
    }
}
