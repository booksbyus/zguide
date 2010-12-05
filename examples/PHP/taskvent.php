<?php 
/*
 *  Task ventilator
 *  Binds PUSH socket to tcp://localhost:5557
 *  Sends batch of tasks to workers via that socket
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

$context = new ZMQContext();

//  Socket to send messages on
$sender = new ZMQSocket($context, ZMQ::SOCKET_PUSH);
$sender->bind("tcp://*:5557");

echo "Press Enter when the workers are ready: ";
$fp = fopen('php://stdin', 'r');
$line = fgets($fp, 512);
fclose($fp);
echo "Sending tasks to workers...", PHP_EOL;

//  The first message is "0" and signals start of batch
$sender->send(0);
    
//  Send 100 tasks
$total_msec = 0;     //  Total expected cost in msecs
for ($task_nbr = 0; $task_nbr < 100; $task_nbr++) {
	//  Random workload from 1 to 100msecs
	$workload = mt_rand(1, 100);
	$total_msec += $workload;
	$sender->send($workload);
	
}

printf ("Total expected cost: %d msec\n", $total_msec);
sleep (1);              //  Give 0MQ time to deliver