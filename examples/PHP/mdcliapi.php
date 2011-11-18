<?php
/* =====================================================================
 * mdcliapi.h
 * 
 * Majordomo Protocol Client API
 * Implements the MDP/Worker spec at http://rfc.zeromq.org/spec:7.

 * ---------------------------------------------------------------------
 * Copyright (c) 1991-2011 iMatix Corporation <www.imatix.com>
 * Copyright other contributors as noted in the AUTHORS file.
 * 
 * This file is part of the ZeroMQ Guide: http://zguide.zeromq.org
 * 
 * This is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by 
 * the Free Software Foundation; either version 3 of the License, or (at 
 * your option) any later version.
 * 
 * This software is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public 
 * License along with this program. If not, see 
 * <http://www.gnu.org/licenses/>.
 * =====================================================================
 * 
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */
include_once "zmsg.php";
include_once "mdp.php";

class MDCli {
	//  Structure of our class
	//  We access these properties only via class methods
	private $broker;
	private $context;
	private $client;	//  Socket to broker
	private $verbose;	//  Print activity to stdout
	private $timeout;	//  Request timeout
	private $retries;	//  Request retries
	
	/**
	 * Constructor
	 *
	 * @param string $broker 
	 * @param boolean $verbose 
	 */
	public function __construct($broker, $verbose = false) {
		$this->broker = $broker;
		$this->context = new ZMQContext();
		$this->verbose = $verbose;
		$this->timeout = 2500;           //  msecs
		$this->retries = 3;              //  Before we abandon
		$this->connect_to_broker();
	}
	
	/**
	 * Connect or reconnect to broker
	 */
	protected function connect_to_broker() {
		if($this->client) {
			unset($this->client);
		}
		$this->client = new ZMQSocket($this->context, ZMQ::SOCKET_REQ);
		$this->client->setSockOpt(ZMQ::SOCKOPT_LINGER, 0);
		$this->client->connect($this->broker);
		if($this->verbose) {
			printf("I: connecting to broker at %s...", $this->broker);
		}
	}
	
	/**
	 * Set request timeout
	 *
	 * @param int $timeout (msecs)
	 */
	public function set_timeout($timeout) {
		$this->timeout = $timeout;
	}
	
	/**
	 * Set request retries
	 *
	 * @param int $retries 
	 */
	public function set_retries($retries) {
		$this->retries = $retries;
	}
	
	/**
	 * Send request to broker and get reply by hook or crook
	 * Takes ownership of request message and destroys it when sent.
	 * Returns the reply message or NULL if there was no reply.
	 *
	 * @param string $service 
	 * @param Zmsg $request 
	 * @param string $client
	 * @return Zmsg
	 */
	public function send($service, Zmsg $request) {
		//  Prefix request with protocol frames
		//  Frame 1: "MDPCxy" (six bytes, MDP/Client	
		//  Frame 2: Service name (printable string)
		$request->push($service);
		$request->push(MDPC_CLIENT);
		if ($this->verbose) {
			printf ("I: send request to '%s' service:", $service);
			echo $request->__toString();
		}
	
		$retries_left = $this->retries;
		$read = $write = array();
		while($retries_left) {
			$request->set_socket($this->client)->send();
			
			 //  Poll socket for a reply, with timeout
			$poll = new ZMQPoll();
			$poll->add($this->client, ZMQ::POLL_IN);
			$events = $poll->poll($read, $write, $this->timeout);
			
			//  If we got a reply, process it
			if($events) {
				$request->recv();
				if ($this->verbose) {
					echo "I: received reply:", $request->__toString(), PHP_EOL;
				}
				//  Don't try to handle errors, just assert noisily
				assert ($request->parts() >= 3);
				
				$header = $request->pop();
				assert($header == MDPC_CLIENT);
				
				$reply_service = $request->pop();
				assert($reply_service == $service);
				
				return $request; //  Success
			} else if($retries_left--) {
				if($this->verbose) {
					echo "W: no reply, reconnecting...", PHP_EOL;
				}
				//  Reconnect, and resend message
				$this->connect_to_broker();
				$request->send();
			} else {
				echo "W: permanent error, abandoning request", PHP_EOL;
				break;	//  Give up
			}
		}
	}
}