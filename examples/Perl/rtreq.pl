#!/usr/bin/perl
=pod

Router-to-request example

Author: Klaas Nijkes

=cut

use strict;
use warnings;
use 5.10.0;

use ZMQ::LibZMQ3;
use ZMQ::Constants qw(ZMQ_ROUTER ZMQ_REQ ZMQ_IDENTITY);

use zhelpers;
use threads;

my $NBR_WORKERS = 10;

sub workerTask {
    my $context = zmq_init();
    my $worker = zmq_socket($context, ZMQ_REQ);
    zmq_connect($worker, "tcp://localhost:5671");
    my $total = 0;
    while (1) {
        # Tell the broker we're ready for work
        s_send($worker, "Hi boss!");

        # Get workload from broker, until finished
        my $workload = s_recv($worker);
        if ($workload && $workload eq 'Fired!') {
            say "Completed $total tasks";
            last;
        }
        $total++;

        # Do some random work
        s_sleep(rand(500));
    }
    zmq_close($worker);
    zmq_term($context);
    threads->exit;
}

my $context = zmq_init();
my $broker = zmq_socket($context, ZMQ_ROUTER);
zmq_bind($broker, "tcp://*:5671");

# While this example runs in a single process, that is just to make
# it easier to start and stop the example. Each thread has its own
# context and conceptually acts as a separate process.
for (1 .. $NBR_WORKERS) {
    threads->create(\&workerTask)->detach();
}

# Run for five seconds and then tell workers to end
my $endTime = time() + 5;
my $workersFired = 0;
while (1) {
    # Next message gives us least recently used worker
    my $identity = s_recv($broker);
    s_sendmore($broker, $identity);
    s_recv($broker); # Envelope
    s_recv($broker); # Response from worker
    s_sendmore($broker, "");

    # Encourage workers until it's time to fire them
    if (time() < $endTime) {
        s_send($broker, 'Work harder');
    } else {
        s_send($broker, 'Fired!');
        last if (++$workersFired == $NBR_WORKERS);
    }
}

zmq_close($broker);
zmq_term($context);

exit(0);
