<?php
/*
 * Custom routing Router to Mama (ROUTER to REQ)
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>a
 */

define("NBR_WORKERS", 10);

function worker_thread() {
	$context = new ZMQContext();
	$worker = new ZMQSocket($context, ZMQ::SOCKET_REQ);
	$worker->connect("ipc://routing.ipc");
	
	$total = 0;
	while(true) {
		//  Tell the router we're ready for work
		$worker->send("ready");
		
		//  Get workload from router, until finished
		$workload = $worker->recv();
		if($workload == 'END') {
			printf ("Processed: %d tasks%s", $total, PHP_EOL);
			break;
		}
		$total++;
		
		//  Do some random work
		usleep(mt_rand(1, 1000000));
	}
}

for ($worker_nbr = 0; $worker_nbr < NBR_WORKERS; $worker_nbr++) {
	if(pcntl_fork() == 0) {
		worker_thread(); 
		exit();
	}
}

$context = new ZMQContext();
$client = $context->getSocket(ZMQ::SOCKET_ROUTER);
$client->bind("ipc://routing.ipc");

for ($task_nbr = 0; $task_nbr < NBR_WORKERS * 10; $task_nbr++) {
	//  LRU worker is next waiting in queue
	$address = $client->recv();
	$empty = $client->recv();
	$read = $client->recv();
	
	$client->send($address, ZMQ::MODE_SNDMORE);
	$client->send("", ZMQ::MODE_SNDMORE);
	$client->send("This is the workload");
}

//  Now ask mamas to shut down and report their results
for ($task_nbr = 0; $task_nbr < NBR_WORKERS; $task_nbr++) {
	//  LRU worker is next waiting in queue
	$address = $client->recv();
	$empty = $client->recv();
	$read = $client->recv();
	
	$client->send($address, ZMQ::MODE_SNDMORE);
	$client->send("", ZMQ::MODE_SNDMORE);
	$client->send("END");
}

sleep (1);              //  Give 0MQ/2.0.x time to flush output
