<?php
/*
 *  Weather update server
 *  Binds PUB socket to tcp://*:5556
 *  Publishes random weather updates
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

//  Prepare our context and publisher
$context = new ZMQContext();
$publisher = $context->getSocket(ZMQ::SOCKET_PUB);
$publisher->bind("tcp://*:5556");
$publisher->bind("ipc://weather.ipc");

while (true) {
	//  Get values that will fool the boss
	$zipcode     = mt_rand(0, 100000);
	$temperature = mt_rand(-80, 135);
	$relhumidity = mt_rand(10, 60);

	//  Send message to all subscribers
	$update = sprintf ("%05d %d %d", $zipcode, $temperature, $relhumidity);
	$publisher->send($update);
}
