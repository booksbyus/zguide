<?php
/*
 *  Task sink
 *  Binds PULL socket to tcp://localhost:5558
 *  Collects results from workers via that socket
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

//  Prepare our context and socket
$context = new ZMQContext();
$receiver = new ZMQSocket($context, ZMQ::SOCKET_PULL);
$receiver->bind("tcp://*:5558");

//  Wait for start of batch
$string = $receiver->recv();

//  Start our clock now
$tstart = microtime(true);

//  Process 100 confirmations
$total_msec = 0;     //  Total calculated cost in msecs
for ($task_nbr = 0; $task_nbr < 100; $task_nbr++) {
	$string = $receiver->recv();
	if($task_nbr % 10 == 0) {
		echo ":";
	} else {
		echo ".";
	}
}

$tend = microtime(true);

$total_msec = ($tend - $tstart) * 1000;
echo PHP_EOL;
printf ("Total elapsed time: %d msec", $total_msec);
echo PHP_EOL;