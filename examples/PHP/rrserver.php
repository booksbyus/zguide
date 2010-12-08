<?php
/*
 * Hello World server
 * Connects REP socket to tcp://*:5560
 * Expects "Hello" from client, replies with "World"
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

$context = new ZMQContext();

//  Socket to talk to clients
$responder = new ZMQSocket($context, ZMQ::SOCKET_REP);
$responder->connect("tcp://localhost:5560");

while(true) {
	//  Wait for next request from client
	$string = $responder->recv();
	printf ("Received request: [%s]%s", $string, PHP_EOL);
	
	// Do some 'work'
	sleep(1);
	
	//  Send reply back to client
	$responder->send("World");
}
