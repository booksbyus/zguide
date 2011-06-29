<?php
/*
 *  Publisher for durable subscriber
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

$context = new ZMQContext(1);

//  Subscriber tells us when it's ready here
$sync = new ZMQSocket($context, ZMQ::SOCKET_PULL);
$sync->bind("htcp://*:5564");

//  We send updates via this socket
$publisher = new ZMQSocket($context, ZMQ::SOCKET_PUB);

//  Prevent publisher overflow from slow subscribers
$publisher->setSockOpt(ZMQ::SOCKOPT_HWM, 1);

//  Specify swap space in bytes, this covers all subscribers
$publisher->setSockOpt(ZMQ::SOCKOPT_SWAP, 25000000);

//  Create an endpoint for accepting connections
$publisher->bind("tcp://*:5565");

//  Wait for synchronization request
$string = $sync->recv();

//  Now broadcast exactly 10 updates with pause
for($update_nbr = 0; $update_nbr < 10; $update_nbr++) {
	$string = sprintf("Update %d", $update_nbr);
	$publisher->send($string);
	sleep(1);
}
$publisher->send("END");

sleep (1);              //  Give 0MQ/2.0.x time to flush output
