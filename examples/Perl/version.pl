# Report 0MQ version in Perl

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;

my ($major, $minor, $patch) = ZMQ::FFI->new->version;
say "Current 0MQ version is $major.$minor.$patch";
