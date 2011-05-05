<?php
/* Titanic client example
 * Implements client side of http://rfc.zeromq.org/spec:9
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */
include_once "mdcliapi.php";

/**
 * Calls a TSP service                                          
 * Returns response if successful (status code 200 OK), else NULL
 *
 * @param string $session 
 * @param string $service 
 * @param Zmsg $request 
 */
function s_service_call($session, $service, $request) {
    $reply = $session->send($service, $request);
    if($reply) {
        $status = $reply->pop();
        if($status == "200") {
            return $reply;
        } else if($status == "404") {
            echo "E: client fatal error, aborting", PHP_EOL;
            exit(1);
        } else if($status == "500") {
            echo "E: server fatal error, aborting", PHP_EOL;
            exit(1);
        }
    } else {
        exit(0);
    }
    
    return NULL; //  Didn't succeed, don't care why not
}

$verbose = $_SERVER['argc'] > 1 && $_SERVER['argv'][1] == '-v';
$session = new Mdcli("tcp://localhost:5555", $verbose);

//  1. Send 'echo' request to Titanic
$request = new Zmsg();
$request->push("Hello world");
$request->push("echo");
$reply = s_service_call($session, "titanic.request", $request);

$uuid = null;
if($reply) {
    $uuid = $reply->pop();
    printf("I: request UUID %s %s", $uuid, PHP_EOL);
}
    
//  2. Wait until we get a reply
while(true) {
    usleep(100000);
    $request = new Zmsg();
    $request->push($uuid);
    $reply = s_service_call ($session, "titanic.reply", $request);
    
    if($reply) {
        $reply_string = $reply->last();
        printf ("Reply: %s %s", $reply_string, PHP_EOL);
        
        //  3. Close request
        $request = new Zmsg();
        $request->push($uuid);
        $reply = s_service_call ($session, "titanic.close", $request);
        break;
    } else {
        echo "I: no reply yet, trying again...", PHP_EOL;
        usleep (5000000);     //  Try again in 5 seconds 
    }
}
