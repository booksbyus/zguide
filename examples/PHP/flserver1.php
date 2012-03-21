<?php
/*
* Freelance server - Model 1
* Trivial echo service
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
	$msg = $server->recvMulti();
	$server->sendMulti($msg);
}