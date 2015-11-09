# Lazy Pirate client in Perl
# Use poll to do a safe request-reply
# To run, start lpserver.pl then randomly kill/restart it

use strict;
use warnings;
use v5.10;

use ZMQ::FFI;
use ZMQ::FFI::Constants qw(ZMQ_REQ);

use EV;

my $REQUEST_TIMEOUT = 2500; # msecs
my $REQUEST_RETRIES = 3;    # Before we abandon
my $SERVER_ENDPOINT = 'tcp://localhost:5555';

my $ctx = ZMQ::FFI->new();
say 'I: connecting to server...';
my $client = $ctx->socket(ZMQ_REQ);
$client->connect($SERVER_ENDPOINT);

my $sequence = 0;
my $retries_left = $REQUEST_RETRIES;

REQUEST_LOOP:
while ($retries_left) {
    # We send a request, then we work to get a reply
    my $request = ++$sequence;
    $client->send($request);

    my $expect_reply = 1;

    RETRY_LOOP:
    while ($expect_reply) {
        # Poll socket for a reply, with timeout
        EV::once $client->get_fd, EV::READ, $REQUEST_TIMEOUT / 1000, sub {
            my ($revents) = @_;

            # Here we process a server reply and exit our loop if the
            # reply is valid. If we didn't get a reply we close the client
            # socket and resend the request. We try a number of times
            # before finally abandoning:

            if ($revents == EV::READ) {
                while ($client->has_pollin) {
                    # We got a reply from the server, must match sequence
                    my $reply = $client->recv();

                    if ($reply == $sequence) {
                        say "I: server replied OK ($reply)";
                        $retries_left = $REQUEST_RETRIES;
                        $expect_reply = 0;
                    }
                    else {
                        say "E: malformed reply from server: $reply";
                    }
                }
            }
            elsif (--$retries_left == 0) {
                say 'E: server seems to be offline, abandoning';
            }
            else {
                say "W: no response from server, retrying...";
                # Old socket is confused; close it and open a new one
                $client->close;
                say "reconnecting to server...";
                $client = $ctx->socket(ZMQ_REQ);
                $client->connect($SERVER_ENDPOINT);
                # Send request again, on new socket
                $client->send($request);
            }
        };

        last RETRY_LOOP if $retries_left == 0;
        EV::run;
    }
}
