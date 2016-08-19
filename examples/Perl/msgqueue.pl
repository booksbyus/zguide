# Simple message queuing broker in Perl
# Same as request-reply broker but using shared queue proxy

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_ROUTER ZMQ_DEALER);

my $context = ZMQ::FFI->new();

# Socket facing clients
my $frontend = $context->socket(ZMQ_ROUTER);
$frontend->bind('tcp://*:5559');

# Socket facing services
my $backend = $context->socket(ZMQ_DEALER);
$backend->bind('tcp://*:5560');

# Start the proxy
$context->proxy($frontend, $backend);

# We never get here...
