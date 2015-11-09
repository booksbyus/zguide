# Lazy Pirate server in Perl
# Binds REQ socket to tcp://*:5555
# Like hwserver except:
#  - echoes request as-is
#  - randomly runs slowly, or exits to simulate a crash.

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_REP);

my $context = ZMQ::FFI->new();
my $server = $context->socket(ZMQ_REP);
$server->bind('tcp://*:5555');

my $cycles = 0;

SERVER_LOOP:
while (1) {
    my $request = $server->recv();
    $cycles++;

    # Simulate various problems, after a few cycles
    if ($cycles > 3 && int(rand(3)) == 0) {
        say "I: simulating a crash";
        last SERVER_LOOP;
    }
    elsif ($cycles > 3 && int(rand(3)) == 0) {
        say "I: simulating CPU overload";
        sleep 2;
    }
    say "I: normal request ($request)";
    sleep 1; # Do some heavy work
    $server->send($request);
}
