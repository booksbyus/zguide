<?php
/*
* Freelance Client - Model 2
* Uses DEALER socket to blast one or more services
*
* Author: Rob Gagnon <rgagnon24(at)gmail(dot)com>
*/

class FLClient {
	const GLOBAL_TIMEOUT = 2500; // ms

	private $servers = 0;
	private $sequence = 0;
	/** @var ZMQContext */
	private $context = null;
	/** @var ZMQSocket */
	private $socket = null;

	public function __construct() {
		$this->servers = 0;
		$this->sequence = 0;
		$this->context = new ZMQContext();
		$this->socket = $this->context->getSocket(ZMQ::SOCKET_DEALER);
	}

	public function __destruct() {
		$this->socket->setSockOpt(ZMQ::SOCKOPT_LINGER, 0);
		$this->socket = null;
		$this->context = null;
	}

	/**
	* @param string $endpoint
	*/
	public function connect($endpoint) {
		$this->socket->connect($endpoint);
		$this->servers++;
		printf("I: Connected to %s\n", $endpoint);
	}

	/**
	* @param string $request
	*/
	public function request($request) {
		// Prefix request with sequence number and empty envelope
		$this->sequence++;
		$msg = array('', $this->sequence, $request);

		// Blast the request to all connected servers
		for($server = 1; $server <= $this->servers; $server++) {
			$this->socket->sendMulti($msg);
		}

		// Wait for a matching reply to arrive from anywhere
		// Since we can poll several times, calculate each one
		$poll = new ZMQPoll();
		$poll->add($this->socket, ZMQ::POLL_IN);

		$reply = null;
		$endtime = time() + self::GLOBAL_TIMEOUT / 1000;
		while (time() < $endtime) {
			$readable = $writable = array();
			$events = $poll->poll($readable, $writable, ($endtime - time()) * 1000);
			foreach($readable as $sock) {
				if ($sock == $this->socket) {
					$reply = $this->socket->recvMulti();
					if (count($reply) != 3) {
						exit;
					}
					$sequence = $reply[1];
					if ($sequence == $this->sequence) {
						break;
					}
				}
			}
		}

		return $reply;
	}
}

$cmd = array_shift($argv);
if (count($argv) == 0) {
	printf("I: syntax: %s <endpoint> ...\n", $cmd);
	exit;
}

// Create new freelance client object
$client = new FLClient();

foreach($argv as $endpoint) {
	$client->connect($endpoint);
}

$start = time();
for($requests = 0; $requests < 10000; $requests++) {
	$request = "random name";
	$reply = $client->request($request);
	if (!isset($reply)) {
		print "E: name service not available, aborting\n";
		break;
	}
}

printf("Average round trip cost: %i ms\n", ((time() - $start) / 100));
$client = null;