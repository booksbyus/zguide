#!/usr/bin/perl
=pod

Report 0MQ version

Author: Sonia Hamilton <sonia@snowfrog.net>

=cut

use strict;
use warnings;

use ZeroMQ qw/:all/;

my ($major, $minor, $patch) = ZeroMQ::version();
print ("Current 0MQ version is $major.$minor.$patch\n");
