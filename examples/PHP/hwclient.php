<?php
/*
 *  Hello World client
 *  Connects REQ socket to tcp://localhost:5555
 *  Sends "Hello" to server, expects "World" back
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

$context = new ZMQContext();

//  Socket to talk to server
echo "Connecting to hello world server...\n";
$requester = new ZMQSocket($context, ZMQ::SOCKET_REQ);
$requester->connect("tcp://localhost:5555");

for($request_nbr = 0; $request_nbr != 10; $request_nbr++) {
	printf ("Sending request %d...\n", $request_nbr);
	$requester->send("Hello");
	
	$reply = $requester->recv();
	printf ("Received reply %d: [%s]\n", $request_nbr, $reply);
}
