<?php
/* 
 * Paranoid Pirate worker
 * 
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */
include "zmsg.php";

define("HEARTBEAT_LIVENESS", 3); //  3-5 is reasonable
define("HEARTBEAT_INTERVAL", 1); //  secs
define("INTERVAL_INIT", 1000); //  Initial reconnect
define("INTERVAL_MAX", 32000); //  After exponential backoff

/* 
 * Helper function that returns a new configured socket
 * connected to the Hello World server
 */
function s_worker_socket($context) {
	$worker = new ZMQSocket($context, ZMQ::SOCKET_DEALER);
	
	//  Set random identity to make tracing easier
	$identity = sprintf ("%04X-%04X", rand(0, 0x10000), rand(0, 0x10000));
	$worker->setSockOpt(ZMQ::SOCKOPT_IDENTITY, $identity);
	$worker->connect("tcp://localhost:5556");

	//  Configure socket to not wait at close time
	$worker->setSockOpt(ZMQ::SOCKOPT_LINGER, 0);
	
    //  Tell queue we're ready for work
    printf ("I: (%s) worker ready%s", $identity, PHP_EOL);
	$worker->send("READY");
	
	return array($worker, $identity);
}

$context = new ZMQContext();
list($worker, $identity) = s_worker_socket($context);

//  If liveness hits zero, queue is considered disconnected
$liveness = HEARTBEAT_LIVENESS;
$interval = INTERVAL_INIT;


//  Send out heartbeats at regular intervals
$heartbeat_at = microtime(true) + HEARTBEAT_INTERVAL;
$read = $write = array();
$poll = new ZMQPoll();
$poll->add($worker, ZMQ::POLL_IN);

$cycles = 0;
while(true) {
	$events = $poll->poll($read, $write, HEARTBEAT_INTERVAL * 1000);
	
	if($events) {
		//  Get message
		//  - 3-part envelope + content -> request
		//  - 1-part "HEARTBEAT" -> heartbeat
		$zmsg = new Zmsg($worker);
		$zmsg->recv();
		
		if($zmsg->parts() == 3) {
			//  Simulate various problems, after a few cycles
			$cycles++;
			if($cycles > 3 && rand(0, 5) == 0) {
				printf ("I: (%s) simulating a crash%s", $identity, PHP_EOL);
				break;
			} else if($cycles > 3 && rand(0, 5) == 0) {
				printf ("I: (%s) simulating CPU overload%s", $identity, PHP_EOL);
				sleep(5);
			}
			printf ("I: (%s) normal reply - %s%s", $identity, $zmsg->body(), PHP_EOL);
			$zmsg->send();
			$liveness = HEARTBEAT_LIVENESS;
			sleep(1); // Do some heavy work
		} else if($zmsg->parts() == 1 && $zmsg->body() == 'HEARTBEAT'){
			$liveness = HEARTBEAT_LIVENESS;
		} else {
			printf ("E: (%s) invalid message%s%s", $identity, PHP_EOL, $zmsg->__toString());
		}
		$interval = INTERVAL_INIT;
	} else if(--$liveness == 0) {
		printf ("W: (%s) heartbeat failure, can't reach queue%s", $identity, PHP_EOL);
		printf ("W: (%s) reconnecting in %d msec...%s", $identity, $interval, PHP_EOL);
		usleep ($interval * 1000 * 1000);

		if ($interval < INTERVAL_MAX) {
			$interval *= 2;
		}
		list($worker, $identity) = s_worker_socket ($context);
        $liveness = HEARTBEAT_LIVENESS;
	}
	
	//  Send heartbeat to queue if it's time
	if(microtime(true) > $heartbeat_at) {
		$heartbeat_at = microtime(true) + HEARTBEAT_INTERVAL;
		printf ("I: (%s) worker heartbeat%s", $identity, PHP_EOL);
		$worker->send("HEARTBEAT");
	}
}
