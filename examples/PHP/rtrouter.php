<?php
/*
 * Cross-connected ROUTER sockets addressing each other
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */
include "zhelpers.php";

$context = new ZMQContext();

$worker = new ZMQSocket($context, ZMQ::SOCKET_ROUTER);
$worker->setSockOpt(ZMQ::SOCKOPT_IDENTITY, "WORKER");
$worker->bind("ipc://rtrouter.ipc");

$server = new ZMQSocket($context, ZMQ::SOCKET_ROUTER);
$server->setSockOpt(ZMQ::SOCKOPT_IDENTITY, "SERVER");
$server->connect("ipc://rtrouter.ipc");

sleep(1);

$server->send("WORKER", ZMQ::MODE_SNDMORE);
$server->send("", ZMQ::MODE_SNDMORE);
$server->send("send to worker");
s_dump($worker);

$worker->send("SERVER", ZMQ::MODE_SNDMORE);
$worker->send("", ZMQ::MODE_SNDMORE);
$worker->send("send to server");
s_dump($server);