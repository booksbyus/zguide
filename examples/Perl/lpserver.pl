#!/usr/bin/perl
=pod

Lazy Pirate server

Like hwserver except:
 - echoes request as-is
 - randomly runs slowly, or exits to simulate a crash.

Author: Michael Gray (mjg17)
See hwserver.pl for original authors.

=cut

use strict;
use warnings;
use 5.010;

use ZMQ::LibZMQ3;
use ZMQ::Constants qw(ZMQ_REP);

my $MAX_MSGLEN = 255;

my $context = zmq_init();

# Socket to talk to clients
my $responder = zmq_socket($context, ZMQ_REP);
zmq_bind($responder, 'tcp://*:5555');

my $cycles = 0;
while (1) {
    # Wait for the next request from client
    my $message;
    my $size = zmq_recv($responder, $message, $MAX_MSGLEN);
    my $request = substr($message, 0, $size);
    $cycles++;

    # Simulate various problems, after a few cycles
    if ($cycles > 6 and int(rand(7)) == 0) {
        say 'I: simulating a crash';
        last;
    }
    elsif ($cycles > 3 and int(rand(4)) == 0) {
        say 'I: simulating CPU overload';
        sleep (2);
    }
    say 'I: normal request: ['. $request .']';

    # Do some 'work'
    sleep (1);

    # Send reply back to client
    zmq_send($responder, $request);
}

exit;
