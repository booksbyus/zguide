<?php
/*
 * Multithreaded Hello World server. Uses proceses due
 * to PHP's lack of threads!
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

function worker_routine() {
	$context = new ZMQContext();
	// Socket to talk to dispatcher
	$receiver = new ZMQSocket($context, ZMQ::SOCKET_REP);
	$receiver->connect("ipc://workers.ipc");
	
	while(true) {
		$string = $receiver->recv();
		printf ("Received request: [%s]%s", $string, PHP_EOL);
		
		// Do some 'work'
		sleep(1);
		
		// Send reply back to client
		$receiver->send("World");
	}
}

//  Launch pool of worker threads
for($thread_nbr = 0; $thread_nbr != 5; $thread_nbr++) {	
	$pid = pcntl_fork();
	if($pid == 0) {
		worker_routine();
		exit();
	}
}

//  Prepare our context and sockets
$context = new ZMQContext();

//  Socket to talk to clients
$clients = new ZMQSocket($context, ZMQ::SOCKET_ROUTER);
$clients->bind("tcp://*:5555");

//  Socket to talk to workers
$workers = new ZMQSocket($context, ZMQ::SOCKET_DEALER);
$workers->bind("ipc://workers.ipc");

//  Connect work threads to client threads via a queue
$device = new ZMQDevice($clients, $workers);
$device->run ();
