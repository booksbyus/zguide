=pod

Helper module for example applications.

Author: Klaas Nijkes

=cut

use ZMQ::LibZMQ3;
use ZMQ::Constants qw(:all);
use Time::HiRes qw(usleep);

my $MAX_MSGLEN = 255;

sub s_recv {
    my $socket = shift;
    my $size = zmq_recv($socket, $buf, $MAX_MSGLEN);
    return undef if ($size < 0);
    return substr($buf, 0, $size);
}

sub s_send {
    my $socket = shift;
    my $string = shift;
    return zmq_send($socket, $string, -1);
}

sub s_sendmore {
    my $socket = shift;
    my $string = shift;
    my $more = shift;
    return zmq_send($socket, $string, -1, ZMQ_SNDMORE);
}

sub s_set_id {
    my $socket = shift;
    my $identity = sprintf("%04X-%04X", int(rand(10000)), int(rand(10000)));
    zmq_setsockopt($socket, ZMQ_IDENTITY, $identity);
}
    
sub s_dump {
    my $socket = shift;
    print "----------------------------------------\n";
    while (1) {
        my $message = zmq_recvmsg($socket);
        my $size = zmq_msg_size($message);
        my $data = zmq_msg_data($message);
        my $isText = 1;
        foreach $byte (split //, $data) {
            if (ord($byte) < 32 || ord($byte) > 127) {
                $isText = 0;
                last;
            }
        }
        my $str = "";
        if ($isText) {
            $str = $data;
        } else {
            foreach $byte (split //, $data) {
                $str .= sprintf("%02X", ord($byte));
            }
        }
        printf("[%03d] $str\n", $size);
        last if (not zmq_getsockopt($socket, ZMQ_RCVMORE))
    }
}

sub s_sleep {
    my $msecs = shift;
    usleep(1000 * int($msecs));
}

1;
