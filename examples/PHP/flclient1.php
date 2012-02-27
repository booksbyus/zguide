<?php
/*
* Freelance Client - Model 1
* Uses REQ socket to query one or more services
*
* Author: Rob Gagnon <rgagnon24(at)gmail(dot)com>
*/

$request_timeout = 1000; // ms
$max_retries = 3; # Before we abandon

/**
* @param ZMQContext $ctx
* @param string $endpoint
* @param string $request
*/
function try_request($ctx, $endpoint, $request) {
	global $request_timeout;

	printf("I: Trying echo service at %s...\n", $endpoint);
	$client = $ctx->getSocket(ZMQ::SOCKET_REQ);
	$client->connect($endpoint);
	$client->send($request);

	$poll = new ZMQPoll();
	$poll->add($client, ZMQ::POLL_IN);
	$readable = $writable = array();

	$events = $poll->poll($readable, $writable, $request_timeout);
	$reply = null;
	foreach($readable as $sock) {
		if ($sock == $client) {
			$reply = $client->recvMulti();
		} else {
			$reply = null;
		}
	}

	$poll->remove($client);
	$poll = null;
	$client = null;
	return $reply;
}

$context = new ZMQContext();
$request = 'Hello world';
$reply = null;

$cmd = array_shift($argv);
$endpoints = count($argv);
if ($endpoints == 0) {
	printf("I: syntax: %s <endpoint> ...\n", $cmd);
	exit;
}

if ($endpoints == 1) {
	// For one endpoint, we retry N times
	$endpoint = $argv[0];
	for($retries = 0; $retries < $max_retries; $retries++) {
		$reply = try_request($context, $endpoint, $request);
		if (isset($reply)) {
			break; // Success
		}
		printf("W: No response from %s, retrying\n", $endpoint);
	}
} else {
	// For multiple endpoints, try each at most once
	foreach($argv as $endpoint) {
		$reply = try_request($context, $endpoint, $request);
		if (isset($reply)) {
			break; // Success
		}
		printf("W: No response from %s\n", $endpoint);
	}
}

if (isset($reply)) {
	print "Service is running OK\n";
}