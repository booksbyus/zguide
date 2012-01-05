<?php 
/*
 * Custom routing Router to Dealer
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */


//  We have two workers, here we copy the code, normally these would
//  run on different boxes...
function worker_a() {
	$context = new ZMQContext();
	$worker = $context->getSocket(ZMQ::SOCKET_DEALER);
	$worker->setSockOpt(ZMQ::SOCKOPT_IDENTITY, "A");
	$worker->connect("ipc://routing.ipc");

	$total = 0;
	while(true) {
		//  We receive one part, with the workload
		$request = $worker->recv();
		if($request == 'END') {
			printf ("A received: %d%s", $total, PHP_EOL);
			break;
		}
		$total++;
	}
}

function worker_b() {
	$context = new ZMQContext();
	$worker = $context->getSocket(ZMQ::SOCKET_DEALER);
	$worker->setSockOpt(ZMQ::SOCKOPT_IDENTITY, "B");
	$worker->connect("ipc://routing.ipc");
	
	$total = 0;
	while(true) {
		//  We receive one part, with the workload
		$request = $worker->recv();
		if($request == 'END') {
			printf ("B received: %d%s", $total, PHP_EOL);
			break;
		}
		$total++;
	}
}

$pid = pcntl_fork();
if($pid == 0) { worker_a(); exit(); }
$pid = pcntl_fork();
if($pid == 0) { worker_b(); exit(); }

$context = new ZMQContext();
$client = new ZMQSocket($context, ZMQ::SOCKET_ROUTER);
$client->bind("ipc://routing.ipc");

//  Wait for threads to stabilize
sleep(1);

//  Send 10 tasks scattered to A twice as often as B
for ($task_nbr = 0; $task_nbr != 10; $task_nbr++) {
	//  Send two message parts, first the address...
	if(mt_rand(0, 2) > 0) {
		$client->send("A", ZMQ::MODE_SNDMORE);
	} else {
		$client->send("B", ZMQ::MODE_SNDMORE);
	}
	//  And then the workload
	$client->send("This is the workload");
}

$client->send("A", ZMQ::MODE_SNDMORE);
$client->send("END");

$client->send("B", ZMQ::MODE_SNDMORE);
$client->send("END");

sleep (1);              //  Give 0MQ/2.0.x time to flush output

