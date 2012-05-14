<?php
/*
* Freelance server - Model 2
* Does some work, replies OK, with message sequencing
*
* Author: Rob Gagnon <rgagnon24(at)gmail(dot)com>
*/

if (count($argv) < 2) {
	printf("I: Syntax: %s <endpoint>\n", $argv[0]);
	exit;
}

$endpoint = $argv[1];
$context = new ZMQContext();
$server = $context->getSocket(ZMQ::SOCKET_REP);
$server->bind($endpoint);

printf("I: Echo service is ready at %s\n", $endpoint);
while(true) {
	$request = $server->recvMulti();
	if (count($request) != 2) {
		// Fail nastily if run against wrong client
		exit(-1);
	}

	$address = $request[0];
	$reply = array($address, 'OK');
	$server->sendMulti($reply);
}