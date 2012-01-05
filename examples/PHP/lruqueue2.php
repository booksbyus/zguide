<?php 
/*
 *  Least-recently used (LRU) queue device
 *  Demonstrates use of the zmsg class
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */
include "zmsg.php";

define("NBR_CLIENTS", 10);
define("NBR_WORKERS", 3);

//  Basic request-reply client using REQ socket
function client_thread() {
	$context = new ZMQContext();
	$client = new ZMQSocket($context, ZMQ::SOCKET_REQ);
	$client->connect("ipc://frontend.ipc");
	
	//  Send request, get reply
	$client->send("HELLO");
	$reply = $client->recv();
	printf("Client: %s%s", $reply, PHP_EOL);
}

//  Worker using REQ socket to do LRU routing
function worker_thread () {
	$context = new ZMQContext();
	$worker = $context->getSocket(ZMQ::SOCKET_REQ);
	$worker->connect("ipc://backend.ipc");

    //  Tell broker we're ready for work
	$worker->send("READY");
	
	while(true) {
		$zmsg = new Zmsg($worker);
		$zmsg->recv();

		// Additional logic to clean up workers. 
		if($zmsg->address() == "END") {
			exit();
		}
		
		printf ("Worker: %s\n", $zmsg->body());
		
		$zmsg->body_set("OK");
		$zmsg->send();
    }
}

function main() {
	for($client_nbr = 0; $client_nbr < NBR_CLIENTS; $client_nbr++) {
		$pid = pcntl_fork();
		if($pid == 0) {
			client_thread();
			return;
		}
	}

	for($worker_nbr = 0; $worker_nbr < NBR_WORKERS; $worker_nbr++) {
		$pid = pcntl_fork();
		if($pid == 0) {
			worker_thread();
			return;
		}
	}
	
	$context = new ZMQContext();
	$frontend = new ZMQSocket($context, ZMQ::SOCKET_ROUTER);
	$backend = new ZMQSocket($context, ZMQ::SOCKET_ROUTER);
	$frontend->bind("ipc://frontend.ipc");
	$backend->bind("ipc://backend.ipc");
	
	//  Logic of LRU loop
    //  - Poll backend always, frontend only if 1+ worker ready
    //  - If worker replies, queue worker as ready and forward reply
    //    to client if necessary
    //  - If client requests, pop next worker and send request to it

    //  Queue of available workers
	$available_workers = 0;
	$worker_queue = array();
	$writeable = $readable = array();
	
	while($client_nbr > 0) {
		$poll = new ZMQPoll();
		
		//  Poll front-end only if we have available workers
		if($available_workers > 0) {
			$poll->add($frontend, ZMQ::POLL_IN);
		}
		
		//  Always poll for worker activity on backend
		$poll->add($backend, ZMQ::POLL_IN);
		$events = $poll->poll($readable, $writeable);

		if($events > 0) {
			foreach($readable as $socket) {
				//  Handle worker activity on backend
				if($socket === $backend) {
					//  Queue worker address for LRU routing
					$zmsg = new Zmsg($socket);
					$zmsg->recv();
					assert($available_workers < NBR_WORKERS);
					$available_workers++;
					array_push($worker_queue, $zmsg->unwrap());
			
					if($zmsg->body() != "READY") {
						$zmsg->set_socket($frontend)->send();
						
						// exit after all messages relayed
						$client_nbr--;
					}
				} else if($socket === $frontend) {
					$zmsg = new Zmsg($socket);
					$zmsg->recv();
					$zmsg->wrap(array_shift($worker_queue), "");
					$zmsg->set_socket($backend)->send();
					$available_workers--; 
				} 
			}
		}
	}
	
	// Clean up our worker processes
	foreach($worker_queue as $worker) {
		$zmsg = new Zmsg($backend);
		$zmsg->body_set('END')->wrap($worker, "")->send();
	}
	
	sleep(1);
}

main();