# Shows how to handle Ctrl-C (SIGINT) and SIGTERM in Perl

use strict;
use warnings;
use v5.10;

use Errno qw(EINTR);

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_REP);

my $interrupted;

$SIG{INT}  = sub { $interrupted = 1; };
$SIG{TERM} = sub { $interrupted = 1; };

my $context = ZMQ::FFI->new();
my $socket  = $context->socket(ZMQ_REP);

$socket->bind('tcp://*:5558');
$socket->die_on_error(0);

while (!$interrupted) {
    $socket->recv();

    if ($socket->last_errno != EINTR) {
        die $socket->last_strerror;
    }
}

warn "interrupt received, killing server...";
