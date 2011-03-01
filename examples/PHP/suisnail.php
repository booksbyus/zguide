<?php
/* Suicidal Snail 
 *
 *  @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
*/

/*  ---------------------------------------------------------------------
 * This is our subscriber
 * It connects to the publisher and subscribes to everything. It 
 * sleeps for a short time between messages to simulate doing too
 * much work. If a message is more than 1 second late, it croaks.
 */
define("MAX_ALLOWED_DELAY", 100); // msecs

function subscriber() {
	$context = new ZMQContext();
	
	// Subscribe to everything
	$subscriber = new ZMQSocket($context, ZMQ::SOCKET_SUB);
	$subscriber->connect("tcp://localhost:5556");
	$subscriber->setSockOpt(ZMQ::SOCKOPT_SUBSCRIBE, "");

	//  Get and process messages
	while(true) {
		$clock = $subscriber->recv();
		//  Suicide snail logic
		if(microtime(true)*100 - $clock*100 > MAX_ALLOWED_DELAY) {
			echo "E: subscriber cannot keep up, aborting", PHP_EOL;
			break;
		}
		
		//  Work for 1 msec plus some random additional time
		usleep(1000 + rand(0, 1000));
	}
}


/* ---------------------------------------------------------------------
 * This is our server task
 * It publishes a time-stamped message to its pub socket every 1ms.
 */
function publisher() {
	$context = new ZMQContext();
	
	//  Prepare publisher
	$publisher = new ZMQSocket($context, ZMQ::SOCKET_PUB);
	$publisher->bind("tcp://*:5556");
	
	while(true) {
		//  Send current clock (msecs) to subscribers
		$publisher->send(microtime(true));
		usleep(1000); //  1msec wait
	}
}


/*
 * This main thread simply starts a client, and a server, and then
 * waits for the client to croak.
 */
$pid = pcntl_fork();
if($pid == 0) {
	publisher(); 
	exit();
}

$pid = pcntl_fork();
if($pid == 0) {
	subscriber(); 
	exit();
}
