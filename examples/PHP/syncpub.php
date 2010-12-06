<?php
/*
 * Synchronized publisher
 *
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

//  We wait for 10 subscribers
define("SUBSCRIBERS_EXPECTED", 10);

$context = new ZMQContext();

//  Socket to talk to clients
$publisher = new ZMQSocket($context, ZMQ::SOCKET_PUB);
$publisher->bind("tcp://*:5561");

//  Socket to receive signals
$syncservice = new ZMQSocket($context, ZMQ::SOCKET_REP);
$syncservice->bind("tcp://*:5562");

//  Get synchronization from subscribers
$subscribers = 0;
while ($subscribers < SUBSCRIBERS_EXPECTED) {
	//  - wait for synchronization request
	$string = $syncservice->recv();
	//  - send synchronization reply
	$syncservice->send("");
	$subscribers++;
}

//  Now broadcast exactly 1M updates followed by END
for ($update_nbr = 0; $update_nbr < 1000000; $update_nbr++) {
	$publisher->send("Rhubarb");
}

$publisher->send("END");

sleep (1);              //  Give 0MQ/2.0.x time to flush output