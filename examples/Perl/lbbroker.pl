# Load-balancing broker
# Clients and workers are shown here in-process

use strict;
use warnings;
use v5.10;

use threads;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_REQ ZMQ_ROUTER);

use AnyEvent;
use EV;

my $NBR_CLIENTS = 10;
my $NBR_WORKERS = 3;

# Basic request-reply client using REQ socket

sub client_task {
    my ($client_nbr) = @_;

    my $context = ZMQ::FFI->new();
    my $client = $context->socket(ZMQ_REQ);

    $client->set_identity("client-$client_nbr");
    $client->connect('ipc://frontend.ipc');

    # Send request, get reply
    $client->send("HELLO");
    my $reply = $client->recv();
    say "Client: $reply";
}

# While this example runs in a single process, that is just to make
# it easier to start and stop the example. Each client_thread has its own
# context and conceptually acts as a separate process.
# This is the worker task, using a REQ socket to do load-balancing.

sub worker_task {
    my ($worker_nbr) = @_;

    my $context = ZMQ::FFI->new();
    my $worker = $context->socket(ZMQ_REQ);

    $worker->set_identity("worker-$worker_nbr");
    $worker->connect('ipc://backend.ipc');

    # Tell broker we're ready for work
    $worker->send('READY');

    while (1) {
        # Read and save all frames, including empty frame and request
        # This example has only one frame before the empty one,
        # but there could be more
        my ($identity, $empty, $request) = $worker->recv_multipart();
        say "Worker: $request";

        # Send reply
        $worker->send_multipart([$identity, '', 'OK']);
    }
}

# This is the main task. It starts the clients and workers, and then
# routes requests between the two layers. Workers signal READY when
# they start; after that we treat them as ready when they reply with
# a response back to a client. The load-balancing data structure is
# just a queue of next available workers.

# Prepare our context and sockets
my $context = ZMQ::FFI->new();
my $frontend = $context->socket(ZMQ_ROUTER);
my $backend = $context->socket(ZMQ_ROUTER);

$frontend->bind('ipc://frontend.ipc');
$backend->bind('ipc://backend.ipc');

my @client_thr;
my $client_nbr;
for (1..$NBR_CLIENTS) {
    push @client_thr, threads->create('client_task', ++$client_nbr);
}

for my $worker_nbr (1..$NBR_WORKERS) {
    threads->create('worker_task', $worker_nbr)->detach();
}

# Here is the main loop for the least-recently-used queue. It has two
# sockets; a frontend for clients and a backend for workers. It polls
# the backend in all cases, and polls the frontend only when there are
# one or more workers ready. This is a neat way to use 0MQ's own queues
# to hold messages we're not ready to process yet. When we get a client
# reply, we pop the next available worker and send the request to it,
# including the originating client identity. When a worker replies, we
# requeue that worker and forward the reply to the original client
# using the reply envelope.

# Queue of available workers
my @workers;

# Only poll for requests from backend until workers are available
my $worker_poller = AE::io $backend->get_fd, 0, \&poll_backend;
my $client_poller;

# Start the loop
EV::run;

# Give client threads time to flush final output after main loop finishes
$_->join() for @client_thr;

sub poll_backend {
    while ($backend->has_pollin) {
        # Handle worker activity on backend

        my $worker_id = $backend->recv();
        if (!@workers) {
            # Poll for clients now that a worker is available
            $client_poller = AE::io $frontend->get_fd, 0, \&poll_frontend;
        }

        # Queue worker identity for load-balancing
        push @workers, $worker_id;

        # Second frame is empty
        $backend->recv();

        # Third frame is READY or else a client reply identity
        my $client_id = $backend->recv();

        # If client reply, send rest back to frontend
        if ($client_id ne 'READY') {
            my ($empty, $reply) = $backend->recv_multipart();
            $frontend->send_multipart([$client_id, '', $reply]);
            --$client_nbr;
        }

        if ($client_nbr == 0) {
            # End the loop after N messages
            EV::break;
        }
    }
}

sub poll_frontend {
    while ($frontend->has_pollin) {
        if (!@workers) {
            # Stop polling clients until more workers becomes available
            undef $client_poller;
            return;
        }

        # Here is how we handle a client request:
        # Get next client request, route to last-used worker
        my ($client_id, $empty, $request) = $frontend->recv_multipart();

        my $worker_id = shift @workers;
        $backend->send_multipart(
            [$worker_id, '', $client_id, '', $request]
        );
    }
}

