<?php

/*
 *  Simple message queuing broker
 *  Same as request-reply broker but using QUEUE device
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

$context = new ZMQContext();

//  Socket facing clients
$frontend = $context->getSocket(ZMQ::SOCKET_ROUTER);
$frontend->bind("tcp://*:5559");

//  Socket facing services
$backend = $context->getSocket(ZMQ::SOCKET_DEALER);
$backend->bind("tcp://*:5560");

//  Start built-in device
$device = new ZMQDevice($frontend, $backend);
$device->run();

//  We never get here...
