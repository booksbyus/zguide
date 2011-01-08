<?php
/*
 * Multithreaded relay. Actually using processes due a lack 
 * of PHP threads.  
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

function step1() {
	$context = new ZMQContext(); 
	// Signal downstream to step 2
	$sender = new ZMQSocket($context, ZMQ::SOCKET_PAIR);
	$sender->connect("ipc://step2.ipc");
	$sender->send("");
}

function step2() {
	$pid = pcntl_fork();
	if($pid == 0) {
		step1();
		exit();
	}
	
	$context = new ZMQContext(); 
	//  Bind to ipc: endpoint, then start upstream thread
	$receiver = new ZMQSocket($context, ZMQ::SOCKET_PAIR);
	$receiver->bind("ipc://step2.ipc");
	
	// Wait for signal
	$receiver->recv();

	// Signal downstream to step 3
	$sender = new ZMQSocket($context, ZMQ::SOCKET_PAIR);
	$sender->connect("ipc://step3.ipc");
	$sender->send("");
}



// Start upstream thread then bind to icp: endpoint
$pid = pcntl_fork();
if($pid == 0) {
	step2();
	exit();
}

$context = new ZMQContext();
$receiver = new ZMQSocket($context, ZMQ::SOCKET_PAIR);
$receiver->bind("ipc://step3.ipc");


// Wait for signal
$receiver->recv();

echo "Test succesful!", PHP_EOL;