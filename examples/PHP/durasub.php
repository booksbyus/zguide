<?php
/*
 *  Durable subscriber
 */

$context = new ZMQContext(1);

//  Connect our subscriber socket
$subscriber = new ZMQSocket($context, ZMQ::SOCKET_SUB);
$subscriber->setSockOpt(ZMQ::SOCKOPT_IDENTITY, "Hello");
$subscriber->setSockOpt(ZMQ::SOCKOPT_SUBSCRIBE, "");
$subscriber->connect("tcp://localhost:5565");


//  Synchronize with publisher
$sync = new ZMQSocket($context, ZMQ::SOCKET_PUSH);
$sync->connect("tcp://localhost:5564");
$sync->send("");

//  Get updates, expect random Ctrl-C death
while(true) {
	$string = $subscriber->recv();
	echo $string, "\n";
	if($string == "END") {
		break;
	}
}