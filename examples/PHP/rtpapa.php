<?php
/*
 * Custom routing Router to Papa (ROUTER to REP)
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>a
 */
include "zhelpers.php";

//  We will do this all in one thread to emphasize the sequence
//  of events...
$context = new ZMQContext();
$client = new ZMQSocket($context, ZMQ::SOCKET_ROUTER);
$client->bind("inproc://routing");

$worker = new ZMQSocket($context, ZMQ::SOCKET_REP);
$worker->setSockOpt(ZMQ::SOCKOPT_IDENTITY, "A");
$worker->connect("inproc://routing");

//  Send papa address, address stack, empty part, and request
$client->send("A", ZMQ::MODE_SNDMORE);
$client->send("address 3", ZMQ::MODE_SNDMORE);
$client->send("address 2", ZMQ::MODE_SNDMORE);
$client->send("address 1", ZMQ::MODE_SNDMORE);
$client->send("", ZMQ::MODE_SNDMORE);
$client->send("This is the workload");

//  Worker should get just the workload
s_dump($worker);

//  We don't play with envelopes in the worker
$worker->send("This is the reply");

//  Now dump what we got off the ROUTER socket...
s_dump($client);
