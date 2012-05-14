<?php
/* 
 * Paranoid Pirate queue
 * 
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */
include "zmsg.php";

define("MAX_WORKERS", 100);
define("HEARTBEAT_LIVENESS", 3); //  3-5 is reasonable
define("HEARTBEAT_INTERVAL", 1); //  secs


class Queue_T implements Iterator{
	private $queue = array();
	
	/* Iterator functions */
	public function rewind() { return reset($this->queue); }
	public function valid() { return current($this->queue); }
	public function key() { return key($this->queue); }
	public function next() { return next($this->queue); }
	public function current() { return current($this->queue); }

	/*  
	 * Insert worker at end of queue, reset expiry
	 * Worker must not already be in queue
	 */
	public function s_worker_append($identity) {
		if(isset($this->queue[$identity])) {
			printf ("E: duplicate worker identity %s", $identity);
		} else {
			$this->queue[$identity] = microtime(true) + HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS;
		}
	}

	/*
	 * Remove worker from queue, if present
	 */
	public function s_worker_delete($identity) {
		unset($this->queue[$identity]);
	}

	/*
	 * Reset worker expiry, worker must be present
	 */
	function s_worker_refresh($identity) {
		if(!isset($this->queue[$identity])) {
			printf ("E: worker %s not ready\n", $identity);
		} else {
			$this->queue[$identity] = microtime(true) + HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS;
		}
	}
	
	/*
	 * Pop next available worker off queue, return identity
	 */
	public function s_worker_dequeue() {
		reset($this->queue);
		$identity = key($this->queue);
		unset($this->queue[$identity]);
		return $identity;
	}

	/* 
	 * Look for & kill expired workers
	 */
	public function s_queue_purge() {
		foreach($this->queue as $id => $expiry) {
			if(microtime(true) > $expiry) {
				unset($this->queue[$id]);
			}
		}
	}
	
	/*
	 * Return the size of the queue
	 */
	public function size() {
		return count($this->queue);
	}
}

//  Prepare our context and sockets
$context = new ZMQContext();
$frontend = new ZMQSocket($context, ZMQ::SOCKET_ROUTER);
$backend = new ZMQSocket($context, ZMQ::SOCKET_ROUTER);
$frontend->bind("tcp://*:5555");    //  For clients
$backend->bind("tcp://*:5556");    //  For workers
$read = $write = array();

//  Queue of available workers
$queue = new Queue_T();

//  Send out heartbeats at regular intervals
$heartbeat_at = microtime(true) +  HEARTBEAT_INTERVAL;

while(true) {
	$poll = new ZMQPoll();
	$poll->add($backend, ZMQ::POLL_IN);
	
	//  Poll frontend only if we have available workers
	if($queue->size()) {
		$poll->add($frontend, ZMQ::POLL_IN);
	}
	
	$events = $poll->poll($read, $write, HEARTBEAT_INTERVAL * 1000 ); // milliseconds
	
	if($events > 0) {
		foreach($read as $socket) {
			$zmsg = new Zmsg($socket);
			$zmsg->recv();
			
			//  Handle worker activity on backend
			if($socket === $backend) {
				$identity = $zmsg->unwrap();
				
				//  Return reply to client if it's not a control message
				if($zmsg->parts() == 1) {
					if($zmsg->address() == "READY") {
						$queue->s_worker_delete($identity);
						$queue->s_worker_append($identity);
 					} else if($zmsg->address() == 'HEARTBEAT') {
						$queue->s_worker_refresh($identity);
					} else {
						printf ("E: invalid message from %s%s%s", $identity, PHP_EOL, $zmsg->__toString());
					}
				} else {
					$zmsg->set_socket($frontend)->send();
					$queue->s_worker_append($identity);
				}
			} else {
				//  Now get next client request, route to next worker
				$identity = $queue->s_worker_dequeue();
				$zmsg->wrap($identity);
				$zmsg->set_socket($backend)->send();
			}
		}		
		
		if(microtime(true) > $heartbeat_at) {
			foreach($queue as $id => $expiry) {
				$zmsg = new Zmsg($backend);
				$zmsg->body_set("HEARTBEAT");
				$zmsg->wrap($identity, NULL);
				$zmsg->send();
			}
			$heartbeat_at = microtime(true) + HEARTBEAT_INTERVAL;
		}
		$queue->s_queue_purge();
	}
}