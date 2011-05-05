<?php
/*
 * Majordomo Protocol client example - asynchronous
 * Uses the mdcli API to hide all MDP aspects
 * 
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */
include_once "mdcliapi2.php";

$verbose = $_SERVER['argc'] > 1 && $_SERVER['argv'][1] == '-v';
$session = new MDCli("tcp://localhost:5555", $verbose);
for($count = 0; $count < 10000; $count++) {
	$request = new Zmsg(); 
	$request->body_set("Hello world");
	$session->send("echo", $request);
}

for($count = 0; $count < 10000; $count++) {
    $reply = $session->recv();
	if(!$reply) {
		break; // Interrupt or failure
	}
}
printf ("%d replies received", $count);
echo PHP_EOL;
