<?php
/*
 *  Reading from multiple sockets
 *  This version uses a simple recv loop
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

//  Prepare our context and sockets
$context = new ZMQContext();

//  Connect to task ventilator
$receiver = new ZMQSocket($context, ZMQ::SOCKET_PULL);
$receiver->connect("tcp://localhost:5557");

//  Connect to weather server
$subscriber = new ZMQSocket($context, ZMQ::SOCKET_SUB);
$subscriber->connect("tcp://localhost:5556");
$subscriber->setSockOpt(ZMQ::SOCKOPT_SUBSCRIBE, "10001");

//  Process messages from both sockets
//  We prioritize traffic from the task ventilator
while(true) {
	//  Process any waiting tasks
	try {
		for($rc = 0; !$rc;) {
			if($rc = $receiver->recv(ZMQ::MODE_NOBLOCK)) {
				// process task
			}
		}
	} catch (ZMQSocketException $e) {
		// do nothing 
	}
	
	try {
		//  Process any waiting weather updates
		for($rc = 0; !$rc;) {
			if($rc = $subscriber->recv(ZMQ::MODE_NOBLOCK)) {
				// process weather update
			}
		}
	} catch (ZMQSocketException $e) {
		// do nothing 
	}
		
	//  No activity, so sleep for 1 msec
	usleep(1);
}