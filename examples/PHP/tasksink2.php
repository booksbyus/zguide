<?php
/*
 *  Task design 2
 *  Adds pub-sub flow to send kill signal to workers
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

$context = new ZMQContext();

//  Socket to receive messages on
$receiver = new ZMQSocket($context, ZMQ::SOCKET_PULL);
$receiver->bind("tcp://*:5558");

//  Socket for worker control
$controller = new ZMQSocket($context, ZMQ::SOCKET_PUB);
$controller->bind("tcp://*:5559");

//  Wait for start of batch
$string = $receiver->recv();

//  Process 100 confirmations
$tstart = microtime(true);
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

//  Send kill signal to workers
$controller->send("KILL");

//  Finished
sleep (1);              //  Give 0MQ time to deliver
