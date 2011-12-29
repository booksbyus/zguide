<?php

/*
 * Round-trip demonstrator
 * 
 * While this example runs in a single process, that is just to make
 * it easier to start and stop the example. Each thread has its own
 * context and conceptually acts as a separate process.
 * 
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */
include "zmsg.php";

function client_task() {
	$context = new ZMQContext();
	$client = new ZMQSocket($context, ZMQ::SOCKET_DEALER);
	$client->setSockOpt(ZMQ::SOCKOPT_IDENTITY, "C");
	$client->connect("tcp://localhost:5555");
	
	echo "Setting up test...", PHP_EOL;
	usleep(10000);
	
	echo "Synchronous round-trip test...", PHP_EOL;
	$start = microtime(true);
	$text = "HELLO";
	for($requests = 0; $requests < 10000; $requests++) {
		$client->send($text);
	    $msg = $client->recv();
	}
	printf (" %d calls/second%s",
		(1000 * 10000) / (int) ((microtime(true) - $start) * 1000), 
		PHP_EOL);
	
	echo "Asynchronous round-trip test...", PHP_EOL;
	$start = microtime(true);
	for($requests = 0; $requests < 100000; $requests++) {
		$client->send($text);
	}
	
	for($requests = 0; $requests < 100000; $requests++) {
		$client->recv();
	}
	
	printf (" %d calls/second%s",
		(1000 * 100000) / (int) ((microtime(true) - $start) * 1000), 
		PHP_EOL);
}


function worker_task() {
	$context = new ZMQContext();
	$worker = new ZMQSocket($context, ZMQ::SOCKET_DEALER);
	$worker->setSockOpt(ZMQ::SOCKOPT_IDENTITY, "W");
	$worker->connect("tcp://localhost:5556");
	while(true) {
		$zmsg = new Zmsg($worker);
		$zmsg->recv();
		$zmsg->send();
	}
}

function broker_task() {
	//  Prepare our context and sockets
	$context = new ZMQContext();
	$frontend = new ZMQSocket($context, ZMQ::SOCKET_ROUTER);
	$backend = new ZMQSocket($context, ZMQ::SOCKET_ROUTER);
	$frontend->bind("tcp://*:5555");
	$backend->bind("tcp://*:5556");
	
	//  Initialize poll set
	$poll = new ZMQPoll();
	$poll->add($frontend, ZMQ::POLL_IN);
	$poll->add($backend, ZMQ::POLL_IN);
	$read = $write = array();
	while(true) {
		$events = $poll->poll($read, $write);
		foreach($read as $socket) {
			$zmsg = new Zmsg($socket);
			$zmsg->recv();
			if($socket === $frontend) {
				$zmsg->push("W");
				$zmsg->set_socket($backend)->send();
			} else if($socket === $backend) {
				$zmsg->pop();
				$zmsg->push("C");
				$zmsg->set_socket($frontend)->send();
			}
			
		}
	}
}

$wpid = pcntl_fork();
if($wpid == 0) {
	worker_task();
	exit;
}
$bpid = pcntl_fork();
if($bpid == 0) {
	broker_task();
	exit;
}
sleep(1);
client_task();
posix_kill($wpid, SIGKILL);
posix_kill($bpid, SIGKILL);