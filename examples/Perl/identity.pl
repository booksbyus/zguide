# Demonstrate request-reply identities in Perl

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_ROUTER ZMQ_REQ ZMQ_IDENTITY);

use zhelpers;

my $context = ZMQ::FFI->new();
my $sink = $context->socket(ZMQ_ROUTER);
$sink->bind('inproc://example');

# First allow 0MQ to set the identity
my $anonymous = $context->socket(ZMQ_REQ);
$anonymous->connect('inproc://example');
$anonymous->send('ROUTER uses a generated 5 byte identity');
zhelpers::dump($sink);

# Then set the identity ourselves
my $identified = $context->socket(ZMQ_REQ);
$identified->set_identity('PEER2');
$identified->connect('inproc://example');
$identified->send("ROUTER socket uses REQ's socket identity");
zhelpers::dump($sink);
