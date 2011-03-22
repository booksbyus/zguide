<?php
/*
 * Lazy Pirate server
 * Binds REQ socket to tcp://*:5555
 * Like hwserver except:
 * - echoes request as-is
 * - randomly runs slowly, or exits to simulate a crash.
 * 
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

$context = new ZMQContext();
$server = new ZMQSocket($context, ZMQ::SOCKET_REP);
$server->bind("tcp://*:5555");

$cycles = 0;
while(true) {
	$request = $server->recv();
	$cycles++;
	
	//  Simulate various problems, after a few cycles
	if($cycles > 3 && rand(0, 3) == 0) {
		echo "I: simulating a crash", PHP_EOL;
		break;
	} else if($cycles > 3 && rand(0, 3) == 0) {
		echo "I: simulating CPU overload", PHP_EOL;
		sleep(5);
	}
	printf ("I: normal request (%s)%s", $request, PHP_EOL);
    sleep(1); // Do some heavy work
	$server->send($request);
}