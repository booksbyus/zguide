# Weather update client in Perl
# Connects SUB socket to tcp://localhost:5556
# Collects weather updates and finds avg temp in zipcode

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_SUB);

# Socket to talk to server
say "Collecting updates from weather station...";

my $context    = ZMQ::FFI->new();
my $subscriber = $context->socket(ZMQ_SUB);
$subscriber->connect("tcp://localhost:5556");

# Subscribe to zipcode, default is NYC, 10001
my $filter = $ARGV[0] // "10001";
$subscriber->subscribe($filter);

# Process 100 updates
my $update_nbr = 100;
my $total_temp = 0;

my ($update, $zipcode, $temperature, $relhumidity);

for (1..$update_nbr) {
    $update = $subscriber->recv();

    ($zipcode, $temperature, $relhumidity) = split ' ', $update;
    $total_temp += $temperature;
}

printf "Average temperature for zipcode '%s' was %dF\n",
    $filter, $total_temp / $update_nbr;
