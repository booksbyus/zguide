#!/usr/bin/env perl

# wesyoung.me / csirtgadgets.org
# updated to use LibZMQ4 - Brian Kelly / fullauto.com

use 5.011; # so we can use 'say'
use strict;
use warnings;

use ZMQ::LibZMQ4;
use ZMQ::Constants qw(:all);

use threads;

use Carp::Assert;

use constant NBR_CLIENTS    => 7;
use constant NBR_WORKERS    => 2;
use constant READY          => "\001";

use constant BACKEND_URL   => 'ipc://backend.ipc';
use constant FRONTEND_URL  => 'ipc://frontend.ipc';

main();

sub client_thread {

    my $i   = shift;

    my $id = 'Client-'.$i;

    my $ctx = zmq_init();
    my $socket = zmq_socket($ctx, ZMQ_REQ);

    my $rv = zmq_setsockopt($socket, ZMQ_IDENTITY, $id);
    assert($rv == 0, 'setting socket options');

    $rv    = zmq_connect($socket,FRONTEND_URL());
    assert($rv == 0, 'connecting client…');

    my $reply;
    while(1){

        say "$id sending Hello";
        my $rv = zmq_msg_send('Hello',$socket);
        assert($rv == 5, 'sending hello');

        say "$id waiting for reply…";
        $reply = zmq_recvmsg($socket);

        assert($reply);

        say "$id got a reply";

    }
}

sub worker_thread {

    my $i   = shift;

    my $id = 'Worker-'.$i;

    my $ctx = zmq_init();
    my $socket = zmq_socket($ctx, ZMQ_REQ);

    my $rv = zmq_setsockopt($socket, ZMQ_IDENTITY, $id);
    assert($rv == 0);

    $rv    = zmq_connect($socket,BACKEND_URL());
    assert($rv == 0,'connecting to backend');

    say "$id sending READY";
    $rv = zmq_msg_send(READY(), $socket);
    assert($rv);

    my $buf = zmq_msg_init();
    while(1){

       say "$id waiting…";

       my @msg=();
       while (1) {
          my $rv = zmq_msg_recv($buf,$socket);
          push @msg, zmq_msg_data($buf) if $rv;
          while (zmq_getsockopt($socket,ZMQ_RCVMORE)) {
             $rv = zmq_msg_recv($buf,$socket);
             push @msg, zmq_msg_data($buf);
          }
          last if $rv==-1 || -1<$#msg;
       }
       if (-1<$#msg) {
          say "$id got: $msg[2] from $msg[0]";
          say "$id sending OK to $msg[0]";
          zmq_msg_send($msg[0],$socket,ZMQ_SNDMORE);
          zmq_msg_send('',$socket,ZMQ_SNDMORE);
          zmq_msg_send('OK',$socket);
       }

    }

}

sub main {

    say 'starting main…';

    my $client_nbr  = NBR_CLIENTS();

    my $ctx = zmq_init();
    my $frontend = zmq_socket($ctx, ZMQ_ROUTER);
    my $backend  = zmq_socket($ctx, ZMQ_ROUTER);

    my $rv  = zmq_bind($frontend,FRONTEND_URL());
    assert($rv == 0);

    $rv     = zmq_bind($backend,BACKEND_URL());
    assert($rv == 0);

    my @threads;
    foreach my $t (1 .. NBR_WORKERS()){
        say 'starting worker: '.$t;
        push(@threads,threads->create('worker_thread',$t));
    }

    foreach my $t (1 .. NBR_CLIENTS()){
        say 'starting client: '.$t;
        push(@threads,threads->create('client_thread',$t));
    }
    my @workers;

    my ($w_addr,$delim,$c_addr,$data);
    my $items = [
        {
            events      => ZMQ_POLLIN,
            socket      => $frontend,
            callback    => sub {

                if (-1<$#workers) {

                    say 'frontend…';

                    my $buf = zmq_msg_init();
                    my @msg=();
                    while (1) {
                       my $rv = zmq_msg_recv($buf,$frontend);
                       push @msg, zmq_msg_data($buf) if $rv;
                       while (zmq_getsockopt($frontend,ZMQ_RCVMORE)) {
                          $rv = zmq_msg_recv($buf,$frontend);
                          push @msg, zmq_msg_data($buf);
                       }
                       last if $rv==-1 || -1<$#msg;
                    }

                    assert($#msg);

                    zmq_msg_send(pop(@workers),$backend,ZMQ_SNDMORE);
                    zmq_msg_send('',$backend,ZMQ_SNDMORE);
                    zmq_msg_send($msg[0],$backend,ZMQ_SNDMORE);
                    zmq_msg_send('',$backend,ZMQ_SNDMORE);
                    zmq_msg_send($msg[2],$backend,);

                }
            },
        },
        {
            events      => ZMQ_POLLIN,
            socket      => $backend,
            callback    => sub {

                say 'backend…';

                my $buf = zmq_msg_init();
                my @msg=();
                while (1) {
                   my $rv = zmq_msg_recv($buf,$backend);
                   push @msg, zmq_msg_data($buf) if $rv;
                   while (zmq_getsockopt($backend,ZMQ_RCVMORE)) {
                      $rv = zmq_msg_recv($buf,$backend);
                      push @msg, zmq_msg_data($buf);
                   }
                   last if $rv==-1 || -1<$#msg;
                }

                assert($#msg);

                assert($#workers < NBR_WORKERS());

                $w_addr = $msg[0];
                push(@workers,$w_addr);

                $delim = $msg[1];
                assert($delim eq '');

                $c_addr = $msg[2];

                if($c_addr ne READY()){
                    $delim = $msg[3];
                    assert ($delim eq '');

                    $data = $msg[4];

                    say 'sending '.$data.' to '.$c_addr;

                    zmq_msg_send($c_addr,$frontend,ZMQ_SNDMORE);
                    zmq_msg_send('',$frontend,ZMQ_SNDMORE);
                    zmq_msg_send($data,$frontend);

                } else {
                    say 'worker checking in: '.$w_addr;
                }
            },
        },
    ];
    while(1){ zmq_poll($items); select undef,undef,undef,0.025; }

    $_->join() for(@threads);
}
