<?php
/*
 * Simple request-reply broker
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

//  Prepare our context and sockets
$context = new ZMQContext();
$frontend = new ZMQSocket($context, ZMQ::SOCKET_ROUTER);
$backend = new ZMQSocket($context, ZMQ::SOCKET_DEALER);
$frontend->bind("tcp://*:5559");
$backend->bind("tcp://*:5560");

//  Initialize poll set
$poll = new ZMQPoll();
$poll->add($frontend, ZMQ::POLL_IN);
$poll->add($backend, ZMQ::POLL_IN);
$readable = $writeable = array();

//  Switch messages between sockets
while(true) {
	$events = $poll->poll($readable, $writeable);
	
	foreach($readable as $socket) {
		if($socket === $frontend) {
			//  Process all parts of the message
			while(true) {
				$message = $socket->recv();
				//  Multipart detection
				$more = $socket->getSockOpt(ZMQ::SOCKOPT_RCVMORE);
				$backend->send($message, $more ? ZMQ::MODE_SNDMORE : null);
				if(!$more) {
					break; //  Last message part
				}
			}
		} 
		else if($socket === $backend) {
			$message = $socket->recv();
			//  Multipart detection
			$more = $socket->getSockOpt(ZMQ::SOCKOPT_RCVMORE);
			$frontend->send($message, $more ? ZMQ::MODE_SNDMORE : null);
			if(!$more) {
				break; //  Last message part
			}
		}
	}
}