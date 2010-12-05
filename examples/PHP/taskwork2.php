<?php
/*
 *  Task worker - design 2
 *  Adds pub-sub flow to receive and respond to kill signal
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

$context = new ZMQContext();

//  Socket to receive messages on
$receiver = new ZMQSocket($context, ZMQ::SOCKET_PULL);
$receiver->connect("tcp://localhost:5557");

//  Socket to send messages to
$sender = new ZMQSocket($context, ZMQ::SOCKET_PUSH);
$sender->connect("tcp://localhost:5558");

//  Socket for control input
$controller = new ZMQSocket($context, ZMQ::SOCKET_SUB);
$controller->connect("tcp://localhost:5559");
$controller->setSockOpt(ZMQ::SOCKOPT_SUBSCRIBE, "");

//  Process messages from receiver and controller
$poll = new ZMQPoll();
$poll->add($receiver, ZMQ::POLL_IN);
$poll->add($controller, ZMQ::POLL_IN);
$readable = $writeable = array();

//  Process messages from both sockets
while (true) {
	$events = $poll->poll($readable, $writeable);
	if($events > 0) {
		foreach($readable as $socket) {
			if($socket === $receiver) {
				$message = $socket->recv();
				//  Simple progress indicator for the viewer
				echo $message, PHP_EOL;

				//  Do the work
				usleep($message * 1000);

			   //  Send results to sink
				$sender->send("");
			}
			//  Any waiting controller command acts as 'KILL'
			else if($socket === $controller) {
				exit();
			}
		}
	}
}