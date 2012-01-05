<?php
/*
 * Demonstrate identities as used by the request-reply pattern.  Run this
 * program by itself.  Note that the utility functions s_ are provided by
 * zhelpers.h.  It gets boring for everyone to keep repeating this code.
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */
include "zhelpers.php";

$context = new ZMQContext();

$sink = new ZMQSocket($context, ZMQ::SOCKET_ROUTER);
$sink->bind("inproc://example");

//  First allow 0MQ to set the identity
$anonymous = new ZMQSocket($context, ZMQ::SOCKET_REQ);
$anonymous->connect("inproc://example");
$anonymous->send("ROUTER uses a generated UUID");
s_dump ($sink);

//  Then set the identity ourself
$identified = new ZMQSocket($context, ZMQ::SOCKET_REQ);
$identified->setSockOpt(ZMQ::SOCKOPT_IDENTITY, "Hello");
$identified->connect("inproc://example");
$identified->send("ROUTER socket uses REQ's socket identity");
s_dump ($sink);