#!/usr/bin/perl
=pod

Cross-connected ROUTER sockets addressing each other

Author: Klaas Nijkes

=cut

use strict;
use warnings;
use 5.10.0;

use zhelpers;

my $context = zmq_init();
my $worker = zmq_socket($context, ZMQ_ROUTER);
zmq_setsockopt($worker, ZMQ_IDENTITY, "WORKER");
zmq_bind($worker, "ipc://router.ipc");

my $server = zmq_socket($context, ZMQ_ROUTER);
zmq_setsockopt($server, ZMQ_IDENTITY, "SERVER");
zmq_connect($server, "ipc://router.ipc");

# Wait for the worker to connect so that when we send a message
# with routing envelope, it will actually match the worker...
sleep (1);

s_sendmore($server, "WORKER");
s_sendmore($server, "");
s_send($server, "Send to worker");
s_dump($worker);

s_sendmore($worker, "SERVER");
s_sendmore($worker, "");
s_send($worker, "Send to server");
s_dump($server);

zmq_close($worker);
zmq_close($server);
zmq_term($context);

exit(0);
