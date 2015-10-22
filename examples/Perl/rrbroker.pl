# Simple request-reply broker in Perl
# Uses AnyEvent to poll the sockets

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_ROUTER ZMQ_DEALER);

use AnyEvent;
use EV;

# Prepare our context and sockets
my $context  = ZMQ::FFI->new();
my $frontend = $context->socket(ZMQ_ROUTER);
my $backend  = $context->socket(ZMQ_DEALER);

$frontend->bind('tcp://*:5559');
$backend->bind('tcp://*:5560');

# Switch messages between sockets

my $frontend_poller = AE::io $frontend->get_fd, 0, sub {
    while ($frontend->has_pollin) {
        my @message = $frontend->recv_multipart();
        $backend->send_multipart(\@message);
    }
};

my $backend_poller = AE::io $backend->get_fd, 0, sub {
    while ($backend->has_pollin) {
        my @message = $backend->recv_multipart();
        $frontend->send_multipart(\@message);
    }
};

EV::run;
