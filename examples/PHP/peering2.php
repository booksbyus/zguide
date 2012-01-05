<?php
/*
 * Broker peering simulation (part 2)
 * Prototypes the request-reply flow
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */
include "zmsg.php";

define("NBR_CLIENTS", 10);
define("NBR_WORKERS", 3);

// Request-reply client using REQ socket
function client_thread($self) {
	$context = new ZMQContext();
	$client = new ZMQSocket($context, ZMQ::SOCKET_REQ);
	$endpoint = sprintf("ipc://%s-localfe.ipc", $self);
	$client->connect($endpoint);
	
	while(true) {
		//  Send request, get reply
		$client->send("HELLO");
		$reply = $client->recv();
		printf("I: client status: %s%s", $reply, PHP_EOL);
	}
}

//  Worker using REQ socket to do LRU routing
function worker_thread ($self) {
	$context = new ZMQContext();
	$worker = $context->getSocket(ZMQ::SOCKET_REQ);
	$endpoint = sprintf("ipc://%s-localbe.ipc", $self);
	$worker->connect($endpoint);

    //  Tell broker we're ready for work
	$worker->send("READY");
	
	while(true) {
		$zmsg = new Zmsg($worker);
		$zmsg->recv();
		
		sleep(1);
		$zmsg->body_fmt("OK - %04x", mt_rand(0, 0x10000));
		$zmsg->send();
    }
}

//  First argument is this broker's name
//  Other arguments are our peers' names
if($_SERVER['argc'] < 2) {
	echo "syntax: peering2 me {you}...", PHP_EOL;
    exit();
}

$self = $_SERVER['argv'][1];

for($client_nbr = 0; $client_nbr < NBR_CLIENTS; $client_nbr++) {
	$pid = pcntl_fork();
	if($pid == 0) {
		client_thread($self);
		return;
	} 
}

for($worker_nbr = 0; $worker_nbr < NBR_WORKERS; $worker_nbr++) {
	$pid = pcntl_fork();
	if($pid == 0) {
		worker_thread($self);
		return;
	} 
}

printf ("I: preparing broker at %s... %s", $self, PHP_EOL);

//  Prepare our context and sockets
$context = new ZMQContext();

//  Bind cloud frontend to endpoint
$cloudfe = $context->getSocket(ZMQ::SOCKET_ROUTER);
$endpoint = sprintf("ipc://%s-cloud.ipc", $self);
$cloudfe->setSockOpt(ZMQ::SOCKOPT_IDENTITY, $self);
$cloudfe->bind($endpoint);

//  Connect cloud backend to all peers
$cloudbe = $context->getSocket(ZMQ::SOCKET_ROUTER);
$cloudbe->setSockOpt(ZMQ::SOCKOPT_IDENTITY, $self);

for ($argn = 2; $argn < $_SERVER['argc']; $argn++) {
	$peer = $_SERVER['argv'][$argn];
	printf ("I: connecting to cloud backend at '%s'%s", $peer, PHP_EOL);
	$endpoint = sprintf("ipc://%s-cloud.ipc", $peer);
	$cloudbe->connect($endpoint);
}
    
//  Prepare local frontend and backend
$localfe = new ZMQSocket($context, ZMQ::SOCKET_ROUTER);
$endpoint = sprintf("ipc://%s-localfe.ipc", $self);
$localfe->bind($endpoint);
$localbe = new ZMQSocket($context, ZMQ::SOCKET_ROUTER);
$endpoint = sprintf("ipc://%s-localbe.ipc", $self);
$localbe->bind($endpoint);

//  Get user to tell us when we can start...
printf ("Press Enter when all brokers are started: ");
$fp = fopen('php://stdin', 'r');
$line = fgets($fp, 512);
fclose($fp);


//  Interesting part
//  -------------------------------------------------------------
//  Request-reply flow
//  - Poll backends and process local/cloud replies
//  - While worker available, route localfe to local or cloud

//  Queue of available workers
$capacity = 0;
$worker_queue = array();
$readable = $writeable = array();

while(true) {
	$poll = new ZMQPoll();
	$poll->add($localbe, ZMQ::POLL_IN);
	$poll->add($cloudbe, ZMQ::POLL_IN);
	$events = 0;
	
	//  If we have no workers anyhow, wait indefinitely
	try {
		$events = $poll->poll($readable, $writeable, $capacity ? 1000000 : -1);
	} catch(ZMQPollException $e) {
		break;
	}
	
	if($events > 0) {
		foreach($readable as $socket) {
			$zmsg = new Zmsg($socket);
			//  Handle reply from local worker
			if($socket === $localbe) {
				$zmsg->recv();
				//  Use worker address for LRU routing
				$worker_queue[] = $zmsg->unwrap();
				$capacity++;
				if($zmsg->address() == "READY") {
					continue;
				}
			}
			//  Or handle reply from peer broker
			else if($socket === $cloudbe) {
				//  We don't use peer broker address for anything
				$zmsg->recv()->unwrap();
			}
			
			//  Route reply to cloud if it's addressed to a broker
			for($argn = 2; $argn < $_SERVER['argc']; $argn++) {
				if($zmsg->address() == $_SERVER['argv'][$argn]) {
					$zmsg->set_socket($cloudfe)->send();
					$zmsg = null;
				}
			}
			
			//  Route reply to client if we still need to
			if($zmsg) {
				$zmsg->set_socket($localfe)->send();
			}
		}
	}
	
	//  Now route as many clients requests as we can handle
	while($capacity) {
		$poll = new ZMQPoll();
		$poll->add($localfe, ZMQ::POLL_IN);
		$poll->add($cloudfe, ZMQ::POLL_IN);
		$reroutable = false;
		$events = $poll->poll($readable, $writeable, 0);
		if($events > 0) {
			foreach($readable as $socket) {
				$zmsg = new Zmsg($socket);
				$zmsg->recv();
				//  We'll do peer brokers first, to prevent starvation
				if($socket === $cloudfe) {
					$reroutable = false;
				} 
				else if($socket === $localfe) {
					$reroutable = true;
				}
				
				//  If reroutable, send to cloud 20% of the time
				//  Here we'd normally use cloud status information
				if($reroutable && $_SERVER['argc'] > 2 && mt_rand(0, 4) == 0) {
					$zmsg->wrap($_SERVER['argv'][mt_rand(2, ($_SERVER['argc']-1))]);
					$zmsg->set_socket($cloudbe)->send();
				} 
				else {
					$zmsg->wrap(array_shift($worker_queue), "");
					$zmsg->set_socket($localbe)->send();
					$capacity--;
				}
			}
		} else {
			break; //  No work, go back to backends
		}
	}
}