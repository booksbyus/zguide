<?php
/*
 * Demonstrate identities as used by the request-reply pattern.  Run this
 * program by itself.  Note that the utility functions s_ are provided by
 * zhelpers.h.  It gets boring for everyone to keep repeating this code.
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

$context = new ZMQContext();

$sink = new ZMQSocket($context, ZMQ::SOCKET_XREP);
$sink->bind("inproc://example");

//  First allow 0MQ to set the identity
$anonymous = new ZMQSocket($context, ZMQ::SOCKET_REQ);
$anonymous->connect("inproc://example");
$anonymous->send("XREP uses a generated UUID");
s_dump ($sink);

//  Then set the identity ourself
$identified = new ZMQSocket($context, ZMQ::SOCKET_REQ);
$identified->setSockOpt(ZMQ::SOCKOPT_IDENTITY, "Hello");
$identified->connect("inproc://example");
$identified->send("XREP socket uses REQ's socket identity");
s_dump ($sink);

/**
 * zhelpers is not exposed to PHP, so here is a brief equivalent.
 */
function s_dump($socket) {
	echo "----------------------------------------", PHP_EOL;
	while(true) {
		$message = $socket->recv();
		$size = strlen($message);
		printf ("[%03d] ", $size);
		$is_text = true;
		for($i = 0; $i < $size; $i++) {
			if(ord($message[$i]) < 32 || ord($message[$i]) > 127) {
				$message = bin2hex($message);
				break;
			}
		}
		
		echo $message . PHP_EOL;
		
		if(!$socket->getSockOpt(ZMQ::SOCKOPT_RCVMORE)) {
			break;
		}
	}
}