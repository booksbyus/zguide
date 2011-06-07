#!/usr/bin/perl
=pod

Weather update client

Connects SUB socket to tcp://localhost:5556

Collects weather updates and finds avg temp in zipcode

Author: Alexander D'Archangel (darksuji) <darksuji(at)gmail(dot)com>

=cut

use strict;
use warnings;
use 5.10.0;

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
my $total_temp = 0;
my $update_count = 100;
for (1 .. $update_count) {
    my $string = $subscriber->recv->data;
    my ($zipcode, $temperature, $relhumidity) = split(/ /, $string);
    $total_temp += $temperature;
}

say "Average temperature for zipcode '$filter' was ".
    int($total_temp / $update_count);
