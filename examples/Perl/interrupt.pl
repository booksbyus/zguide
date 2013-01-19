#!/usr/bin/perl
=pod

Handle Ctrl-C

Author: Klaas Nijkes

=cut

use strict;
use warnings;
use 5.10.0;

use ZMQ::LibZMQ3;
use ZMQ::Constants qw(ZMQ_REP);

my $MAX_MSGLEN = 255;

my $interrupted = 0;
$SIG{'INT'} = \&intHandler;

sub intHandler {
	$interrupted = 1;
}

my $context = zmq_init();
my $receiver = zmq_socket($context, ZMQ_REP);
zmq_connect($receiver, "tcp://*:5558");

while (1) {
	my $message;
	my $len = zmq_recv($receiver, $message, $MAX_MSGLEN);
	if ($len < 0 && $interrupted) {
		say "Interrupt received, bailing out..";
		last;
	}
}
