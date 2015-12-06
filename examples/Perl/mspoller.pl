# Reading from multiple sockets in Perl
# This version uses AnyEvent to poll the sockets

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_PULL ZMQ_SUB);

use AnyEvent;
use EV;

# Connect to the task ventilator
my $context = ZMQ::FFI->new();
my $receiver = $context->socket(ZMQ_PULL);
$receiver->connect('tcp://localhost:5557');

# Connect to weather server
my $subscriber = $context->socket(ZMQ_SUB);
$subscriber->connect('tcp://localhost:5556');
$subscriber->subscribe('10001');

my $pull_poller = AE::io $receiver->get_fd, 0, sub {
    while ($receiver->has_pollin) {
        my $msg = $receiver->recv();
        # Process task
    }
};

my $sub_poller = AE::io $subscriber->get_fd, 0, sub {
    while ($subscriber->has_pollin) {
        my $msg = $subscriber->recv();
        # Process weather update
    }
};

EV::run;
