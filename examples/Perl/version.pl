#!/usr/bin/perl
=pod

Report 0MQ version

Author: Daisuke Maki (lestrrat)
Original Author: Sonia Hamilton <sonia@snowfrog.net>

=cut

use strict;
use warnings;

use ZMQ::LibZMQ2;

my ($major, $minor, $patch) = ZMQ::LibZMQ2::zmq_version();
print ("Current 0MQ version is $major.$minor.$patch\n");
