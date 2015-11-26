#!/usr/bin/env perl

# wesyoung.me / csirtgadgets.org

use 5.011;
use strict;
use warnings;

BEGIN {
    $ENV{ PERL_ZMQ_BACKEND } = 'ZMQ::LibZMQ3';
}

use ZMQ;
use ZMQ::LibZMQ3;
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
    
    my $ctx = ZMQ::Context->new();
    my $socket = $ctx->socket(ZMQ_REQ);
    
    my $rv = $socket->setsockopt(ZMQ_IDENTITY,$id);
    assert($rv == 0, 'setting socket options');
    
    $rv     = $socket->connect(FRONTEND_URL());
    assert($rv == 0, 'connecting client...');
    
    my $reply;
    while(1){
        say "id sending Hello";
        $rv = $socket->sendmsg('HELLO');
        assert($rv == 5, 'sending hello');
        
        say "$id waiting for reply...";
        $reply = $socket->recvmsg();
        
        assert($reply);
        
        say "$id got a reply";
        
    }
}

sub worker_thread {
    my $i   = shift;

    my $id = 'Worker-'.$i;

    my $ctx     = ZMQ::Context->new();
    my $socket  = $ctx->socket(ZMQ_REQ);
    
    my $rv      = $socket->setsockopt(ZMQ_IDENTITY,$id);
    assert($rv == 0);
    
    $rv         = $socket->connect(BACKEND_URL());
    assert($rv == 0,'connecting to backend');
    
    say "$id sending READY";
    $rv = $socket->sendmsg(READY());
    assert($rv);

    while(1){
        say "$id waiting...";
        
        my @msg = $socket->recv_multipart();
        
        say "$id got: ".$msg[2]->data();
        
        say "$id sending OK";
        
        $socket->send_multipart([ $msg[0]->data(), '', 'OK' ]);
    }
  
}

sub main {
    say 'starting main...';
    
    my $client_nbr  = NBR_CLIENTS();
    
    my $ctx         = ZMQ::Context->new();
    my $frontend    = $ctx->socket(ZMQ_ROUTER);
    my $backend     = $ctx->socket(ZMQ_ROUTER);
    
    my $rv  = $frontend->bind(FRONTEND_URL());
    assert($rv == 0);
    
    $rv     = $backend->bind(BACKEND_URL());
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
            socket      => $frontend->{'_socket'},
            callback    => sub {
                if($#workers > -1){
                    say 'frontend...';
                    
                    my @msg = $frontend->recv_multipart();
                    assert($#msg);
                    
                    $backend->send_multipart([ 
                        pop(@workers),
                        '', 
                        $msg[0]->data(),
                        '', 
                        $msg[2]->data()
                    ]);
                }
            },
        },
        {
            events      => ZMQ_POLLIN,
            socket      => $backend->{'_socket'},
            callback    => sub {
                say 'backend...';
 
                my @msg = $backend->recv_multipart();
                assert($#msg);
                
                assert($#workers < NBR_WORKERS());
                
                $w_addr = $msg[0]->data();
                push(@workers,$w_addr);
                
                $delim = $msg[1]->data();
                assert($delim eq '');
                
                $c_addr = $msg[2]->data();

                if($c_addr ne READY()){
                    $delim = $msg[3]->data();
                    assert ($delim eq '');
                    
                    $data = $msg[4]->data();

                    say 'sending '.$data.' to '.$c_addr;
                    $frontend->send_multipart([ $c_addr, '', $data ]);

                } else {
                    say 'worker checking in: '.$w_addr;
                }
            },
        },
    ];
    while(1){ zmq_poll($items); select undef,undef,undef,0.025; }
 
    $_->join() for(@threads);
}
