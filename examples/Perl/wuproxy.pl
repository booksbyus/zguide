# Weather proxy device in Perl

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_XSUB ZMQ_XPUB);

my $context = ZMQ::FFI->new();

# This is where the weather server sits
my $frontend = $context->socket(ZMQ_XSUB);
$frontend->connect('tcp://192.168.55.210:5556');

# This is our public endpoing fro subscribers
my $backend = $context->socket(ZMQ_XPUB);
$backend->bind('tcp://10.1.1.0:8100');

# Run the proxy until the user interrupts us
$context->proxy($frontend, $backend);
