#!/usr/bin/perl
=pod

Weather update client

Connects SUB socket to tcp://localhost:5556

Collects weather updates and finds avg temp in zipcode

Based on examples/C/wuclient.c; translated to Perl by darksuji

=cut

use strict;
use warnings;
use feature ':5.10';

use ZeroMQ qw/:all/;

my $context = ZeroMQ::Context->new();

# Socket to talk to server
say 'Collecting updates from weather server...';
my $subscriber = $context->socket(ZMQ_SUB);
$subscriber->connect('tcp://localhost:5556');

# Subscribe to zipcode, default is NYC, 10001
my $filter = @ARGV ? $ARGV[0] : '10001 ';
$subscriber->setsockopt(ZMQ_SUBSCRIBE, $filter);

# Process 100 updates
my $update_count = 0;
my $total_temp = 0;
while ($update_count++ < 100) {
    my $string = $subscriber->recv->data;
    my ($zipcode, $temperature, $relhumidity) = split(/ /, $string);
    $total_temp += $temperature;
}

say "Average temperature for zipcode '$filter' was ".
    int($total_temp / $update_count);
