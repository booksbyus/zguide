<?php
/*
 *  Broker peering simulation (part 1)
 *  Prototypes the state flow
 */

//  First argument is this broker's name
//  Other arguments are our peers' names
if($_SERVER['argc'] < 2) {
	echo "syntax: peering1 me {you}...", PHP_EOL;
    exit();
}
$self = $_SERVER['argv'][1];
printf ("I: preparing broker at %s... %s", $self, PHP_EOL);

//  Prepare our context and sockets
$context = new ZMQContext();

//  Bind statebe to endpoint
$statebe = $context->getSocket(ZMQ::SOCKET_PUB);
$endpoint = sprintf("ipc://%s-state.ipc", $self);
$statebe->bind($endpoint);

//  Connect statefe to all peers
$statefe = $context->getSocket(ZMQ::SOCKET_SUB);
$statefe->setSockOpt(ZMQ::SOCKOPT_SUBSCRIBE, "");

for ($argn = 2; $argn < $_SERVER['argc']; $argn++) {
	$peer = $_SERVER['argv'][$argn];
	printf ("I: connecting to state backend at '%s'%s", $peer, PHP_EOL);
	$endpoint = sprintf("ipc://%s-state.ipc", $peer);
	$statefe->connect($endpoint);
}

$readable = $writeable = array();

//  Send out status messages to peers, and collect from peers
//  The zmq_poll timeout defines our own heartbeating
while (true) {
	//  Initialize poll set
	$poll = new ZMQPoll();
	$poll->add($statefe, ZMQ::POLL_IN);
	//  Poll for activity, or 1 second timeout
	$events = $poll->poll($readable, $writeable, 1000);
	
	if($events > 0) {
		//  Handle incoming status message
		foreach($readable as $socket) {
			$address = $socket->recv();
			$body = $socket->recv();
			printf ("%s - %s workers free%s", $address, $body, PHP_EOL);
		}
	} 
	else {
		//  We stick our own address onto the envelope
		$statebe->send($self, ZMQ::MODE_SNDMORE);
		//  Send random value for worker availability
		$statebe->send(mt_rand(1, 10));
		
	}
}
//  We never get here 