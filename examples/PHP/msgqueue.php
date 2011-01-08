<?php

/*
 *  Simple message queuing broker
 *  Same as request-reply broker but using QUEUE device
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

$context = new ZMQContext();

//  Socket facing clients
$frontend = $context->getSocket(ZMQ::SOCKET_XREP);
$frontend->bind("tcp://*:5559");

//  Socket facing services
$backend = $context->getSocket(ZMQ::SOCKET_XREQ);
$backend->bind("tcp://*:5560");

//  Start built-in device
new ZMQDevice(ZMQ::DEVICE_QUEUE, $frontend, $backend);

//  We never get here...
