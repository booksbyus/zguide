<?php
/*
 * Simple Pirate worker
 * Connects REQ socket to tcp://*:5556
 * Implements worker part of LRU queueing
 * 
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */
include "zmsg.php";

$context = new ZMQContext();
$worker = new ZMQSocket($context, ZMQ::SOCKET_REQ);

//  Set random identity to make tracing easier
$identity = sprintf ("%04X-%04X", rand(0, 0x10000), rand(0, 0x10000));
$worker->setSockOpt(ZMQ::SOCKOPT_IDENTITY, $identity);
$worker->connect("tcp://localhost:5556");

//  Tell queue we're ready for work
printf ("I: (%s) worker ready%s", $identity, PHP_EOL);
$worker->send("READY");

$cycles = 0;
while(true) {
	$zmsg = new Zmsg($worker);
	$zmsg->recv();
	$cycles++;
	
	//  Simulate various problems, after a few cycles
	if($cycles > 3 && rand(0, 3) == 0) {
		echo "I: (%s) simulating a crash", $identity, PHP_EOL;
		break;
	} else if($cycles > 3 && rand(0, 3) == 0) {
		echo "I: (%s) simulating CPU overload", $identity, PHP_EOL;
		sleep(5);
	}
	printf ("I: (%s) normal reply - %s%s", $identity, $zmsg->body(), PHP_EOL);
	sleep(1); // Do some heavy work
	$zmsg->send();
}