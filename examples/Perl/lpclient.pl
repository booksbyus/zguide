#!/usr/bin/perl
=pod

    Lazy Pirate client

    Use zmq_poll to do a safe request-reply

    To run, start lpserver and then randomly kill/restart it

Author: Michael Gray (mjg17)

=cut

use strict;
use warnings;
use 5.010;

use ZMQ::LibZMQ3;
use ZMQ::Constants qw(ZMQ_REQ ZMQ_LINGER ZMQ_POLLIN);

use sigtrap qw/handler signal_handler normal-signals/;

my $REQUEST_TIMEOUT = 2500;
my $REQUEST_RETRIES =    3;
my $SERVER_ENDPOINT = 'tcp://localhost:5555';

my $MAX_MSGLEN = 255;

my $interrupted = 0;
sub signal_handler {
    $interrupted = 1;
    say 'W: Interrupt received';
    return;
}

my ($context, $socket);         # globals for simplicity

# Socket to talk to server
sub req_connect {
    $socket = zmq_socket($context, ZMQ_REQ);
    zmq_setsockopt($socket, ZMQ_LINGER, 0); # so we can exit without hanging
    zmq_connect($socket, $SERVER_ENDPOINT);
    return;
}

sub lp_send {
    my ($request) = @_;

  RETRY: foreach my $attempt (1..$REQUEST_RETRIES) {

      zmq_send($socket, $request);

      # Poll socket for a reply, with timeout
      my $reply_msg;
      my $pollitem = {
          socket   => $socket,
          events   => ZMQ_POLLIN,
          callback => sub {
              $reply_msg = zmq_recvmsg($socket);
              return;
          },
      };
      my @prv = zmq_poll([ $pollitem ], $REQUEST_TIMEOUT);
      if (@prv and $prv[0] and $reply_msg) {
          # Success
          my $reply = zmq_msg_data($reply_msg);
          return $reply;
      }

      # Work out what went wrong
      unless (@prv) {
          # interrupted, or error
          return if $interrupted;
          say "E: zmq_poll, '$!'";
          next RETRY;
      }
      if ($prv[0]) {
          # zmq_poll succeeded but zmq_recvmsg didn't (unlikely?)
          say "E: zmq_recvmsg, '$!'";
          next RETRY;
      }
      say 'W: timeout waiting for reply';
      # so continue...

  } continue {
      say 'W: no response from server, retrying...';
      #  Old socket is confused; close it and open a new one
      zmq_close($socket);
      say 'I: reconnecting to server...';
      req_connect;
  }

    say 'E: server seems to be offline, abandoning';
    return;
}

sub main {
    $context = zmq_init();

    say 'I: connecting to server...';
    req_connect;

    my $sequence = 0;

    while (not $interrupted) {

        my $request = ++$sequence;
        my $reply = lp_send($request);
        last unless $reply;

        if ($reply == $request) {
            say "I: server replied ok [$reply]";
        } else {
            say "E: malformed reply from server: '$reply'";
        }
    }

    zmq_close($socket);
    zmq_ctx_destroy($context);

    return;
}

main();

exit 0;
