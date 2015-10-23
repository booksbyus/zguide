package zhelpers;

use strict;
use warnings;
use v5.10;

sub dump {
    my ($socket) = @_;

    say "----------------------------------------";

    for my $message ($socket->recv_multipart()) {
        my $msg_len = length($message);

        my $is_text = 1;

        CHECK_TEXT:
        for (my $i = 0; $i < $msg_len; $i++) {
            my $c = ord(substr($message, $i, 1));

            if ($c < 32 || $c > 126) {
                $is_text = 0;
                last CHECK_TEXT;
            }
        }

        printf "[%03d] ", $msg_len;

        if ($is_text) {
            say $message;
        }
        else {
            say uc(unpack("H*", $message));
        }
    }

}

1;
