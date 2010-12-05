<?php 
/*
 *  Reading from multiple sockets
 *  This version uses zmq_poll()
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

$context = new ZMQContext();

//  Connect to task ventilator
$receiver = new ZMQSocket($context, ZMQ::SOCKET_PULL);
$receiver->connect("tcp://localhost:5557");

//  Connect to weather server
$subscriber = new ZMQSocket($context, ZMQ::SOCKET_SUB);
$subscriber->connect("tcp://localhost:5556");
$subscriber->setSockOpt(ZMQ::SOCKOPT_SUBSCRIBE, "10001");

//  Initialize poll set
$poll = new ZMQPoll();
$poll->add($receiver, ZMQ::POLL_IN);
$poll->add($subscriber, ZMQ::POLL_IN);

$readable = $writeable = array();

//  Process messages from both sockets
while(true) {
	$events = $poll->poll($readable, $writeable);
	if($events > 0) {
		foreach($readable as $socket) {
			if($socket === $receiver) {
				$message = $socket->recv();
				// Process task
			} 
			else if($socket === $subscriber) {
				$mesage = $socket->recv();
				// Process weather update
			}
		}
	}
}
   
//  We never get here