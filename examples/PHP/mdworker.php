<?php
/* 
 * Majordomo Protocol worker example
 * Uses the mdwrk API to hide all MDP aspects
 * 
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

include_once "mdwrkapi.php";

$verbose = $_SERVER['argc'] > 1 && $_SERVER['argv'][1] == "-v";

$mdwrk = new Mdwrk("tcp://localhost:5555", "echo", $verbose);

$reply = NULL;
while(true) {
    $request = $mdwrk->recv($reply);
    $reply = $request;      //  Echo is complex... :-)
}