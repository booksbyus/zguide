<?php
/*
 *  Least-recently used (LRU) queue device
 *  Clients and workers are shown here as IPC as PHP
 *  does not have threads. 
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */
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
		//  Read and save all frames until we get an empty frame
        //  In this example there is only 1 but it could be more
		$address = $worker->recv();
		
		// Additional logic to clean up workers. 
		if($address == "END") {
			exit();
		}
		$empty = $worker->recv();
		assert(empty($empty));
		
		//  Get request, send reply
		$request = $worker->recv();
		printf ("Worker: %s%s", $request, PHP_EOL);
		
		$worker->send($address, ZMQ::MODE_SNDMORE);
		$worker->send("", ZMQ::MODE_SNDMORE);
		$worker->send("OK");
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
					$worker_addr = $socket->recv();
					assert($available_workers < NBR_WORKERS);
					$available_workers++;
					array_push($worker_queue, $worker_addr);
			
					//  Second frame is empty
					$empty = $socket->recv();
					assert(empty($empty));
				
					//  Third frame is READY or else a client reply address
					$client_addr = $socket->recv();
				
					if($client_addr != "READY") {
						$empty = $socket->recv();
						assert(empty($empty));
						$reply = $socket->recv();
						$frontend->send($client_addr, ZMQ::MODE_SNDMORE);
						$frontend->send("", ZMQ::MODE_SNDMORE);
						$frontend->send($reply);
						
						// exit after all messages relayed
						$client_nbr--;
					}
				} else if($socket === $frontend) {
					//  Now get next client request, route to LRU worker
					//  Client request is [address][empty][request]
					$client_addr = $socket->recv();
					$empty = $socket->recv();
					assert(empty($empty));
					$request = $socket->recv();

					$backend->send(array_shift($worker_queue), ZMQ::MODE_SNDMORE);
					$backend->send("", ZMQ::MODE_SNDMORE);
					$backend->send($client_addr, ZMQ::MODE_SNDMORE);
					$backend->send("", ZMQ::MODE_SNDMORE);
					$backend->send($request);

					$available_workers--; 
				} 
			}
		}
	}
	
	// Clean up our worker processes
	foreach($worker_queue as $worker) {
		$backend->send($worker, ZMQ::MODE_SNDMORE);
		$backend->send("", ZMQ::MODE_SNDMORE);
		$backend->send('END');
	}
	
	sleep(1);
}

main();

