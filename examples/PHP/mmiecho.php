<?php
/*
 * MMI echo query example
 * 
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */
include "mdcliapi.php";

$verbose = $_SERVER['argc'] > 1 && $_SERVER['argv'][1] == '-v';
$session = new MDCli("tcp://localhost:5555", $verbose);

//  This is the service we want to look up
$request = new Zmsg();
$request->body_set("echo");

//  This is the service we send our request to
$reply = $session->send("mmi.service", $request);

if($reply) {
    $reply_code = $reply->pop();
    printf ("Lookup echo service: %s %s", $reply_code, PHP_EOL);
}