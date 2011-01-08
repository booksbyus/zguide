<?php
/*
 * Synchronized subscriber
 *
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

$context = new ZMQContext();

//  First, connect our subscriber socket
$subscriber = $context->getSocket(ZMQ::SOCKET_SUB);
$subscriber->connect("tcp://localhost:5561");
$subscriber->setSockOpt(ZMQ::SOCKOPT_SUBSCRIBE, "");

//  Second, synchronize with publisher
$syncclient = $context->getSocket(ZMQ::SOCKET_REQ);
$syncclient->connect("tcp://localhost:5562");

//  - send a synchronization request
$syncclient->send("");

//  - wait for synchronization reply
$string = $syncclient->recv();

//  Third, get our updates and report how many we got
$update_nbr = 0;
while (true) {
	$string = $subscriber->recv();
	if($string == "END") {
		break;
	}
	$update_nbr++;
}
printf ("Received %d updates %s", $update_nbr, PHP_EOL);