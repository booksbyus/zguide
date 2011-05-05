<?php
/*
 * Majordomo Protocol client example
 * Uses the mdcli API to hide all MDP aspects
 * 
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */
include_once "mdcliapi.php";

$verbose = $_SERVER['argc'] > 1 && $_SERVER['argv'][1] == '-v';
$session = new MDCli("tcp://localhost:5555", $verbose);
for($count = 0; $count < 100000; $count++) {
	$request = new Zmsg(); 
	$request->body_set("Hello world");
	$reply = $session->send("echo", $request);
	if(!$reply) {
		break; // Interrupt or failure
	}
}
printf ("%d requests/replies processed", $count);
echo PHP_EOL;
