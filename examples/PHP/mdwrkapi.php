<?php
/* =====================================================================
 * mdwrkapi.php
 * 
 * Majordomo Protocol Worker API
 * Implements the MDP/Worker spec at http://rfc.zeromq.org/spec:7.
 * 
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
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */

include_once "zmsg.php";
include_once "mdp.php";

//  Reliability parameters
define("HEARTBEAT_LIVENESS", 3); //  3-5 is reasonable



//  Structure of our class
//  We access these properties only via class methods
class MDWrk {
    private $ctx;           //  Our context
    private $broker;
    private $service;   
    private $worker;        //  Socket to broker
    private $verbose = false;       //  Print activity to stdout
    
    //  Heartbeat management
    private $heartbeat_at;  //  When to send HEARTBEAT
    private $liveness;      //  How many attempts left
    private $heartbeat;     //  Heartbeat delay, msecs
    private $reconnect;     //  Reconnect delay, msecs
    
    //  Internal state
    private $expect_reply = 0;
    
    //  Return address, if any
    private $reply_to;

	/**
	 * Constructor
	 *
	 * @param string $broker
	 * @param string $service
	 * @param boolean $verbose 
	 */
    public function __construct($broker, $service, $verbose = false) {
        $this->ctx = new ZMQContext();
        $this->broker = $broker;
        $this->service = $service;
        $this->verbose = $verbose;
        $this->heartbeat = 2500; // msecs
        $this->reconnect = 2500; // msecs

        $this->connect_to_broker();
    }
    
    /**
	 * Send message to broker
     * If no msg is provided, creates one internally
	 *
	 * @param string $command
	 * @param string $option
	 * @param Zmsg $msg 
	 */
    public function send_to_broker($command, $option, $msg = null) {
        $msg = $msg ? $msg : new Zmsg();
        
        if($option) {
            $msg->push($option);
        }
        $msg->push($command);
        $msg->push(MDPW_WORKER);
        $msg->push("");
        
        if($this->verbose) {
            printf("I: sending %s to broker %s", $command, PHP_EOL);
            echo $msg->__toString();
        }
        
        $msg->set_socket($this->worker)->send();
    }
    
    /**
     * Connect or reconnect to broker
     */
    public function connect_to_broker() {
        $this->worker = new ZMQSocket($this->ctx, ZMQ::SOCKET_DEALER);
        $this->worker->connect($this->broker);
        if($this->verbose) {
            printf("I: connecting to broker at %s... %s", $this->broker, PHP_EOL);
        }
        
        //  Register service with broker
        $this->send_to_broker(MDPW_READY, $this->service, NULL);
        
        //  If liveness hits zero, queue is considered disconnected
        $this->liveness = HEARTBEAT_LIVENESS;
        $this->heartbeat_at = microtime(true) + ($this->heartbeat / 1000);
    }
    
    /**
	 * Set heartbeat delay
	 *
	 * @param int $heartbeat 
	 */
    public function set_heartbeat($heartbeat) {
        $this->heartbeat = $heartbeat;
    }
    
    /**
     * Set reconnect delay
	 *
	 * @param int $reconnect 
     */
    public function set_reconnect($reconnect) {
        $this->reconnect = $reconnect;
    }
    
    /**
     * Send reply, if any, to broker and wait for next request.
	 *
	 * @param Zmsg $reply 
	 * @return Zmsg Returns if there is a request to process
     */
    public function recv($reply = null) {
        //  Format and send the reply if we were provided one
        assert ($reply || !$this->expect_reply);
        if($reply) {
            $reply->wrap($this->reply_to);
            $this->send_to_broker(MDPW_REPLY, NULL, $reply);
        }
        $this->expect_reply = true;
        
        $read = $write = array();
        while(true) {
            $poll = new ZMQPoll();
            $poll->add($this->worker, ZMQ::POLL_IN);
            $events = $poll->poll($read, $write, $this->heartbeat);

            if($events) {
                $zmsg = new Zmsg($this->worker);
                $zmsg->recv();
                
                if($this->verbose) {
                    echo "I: received message from broker:", PHP_EOL;
                    echo $zmsg->__toString();
                }
                
                $this->liveness = HEARTBEAT_LIVENESS;
                
                //  Don't try to handle errors, just assert noisily
                assert ($zmsg->parts() >= 3);

                $zmsg->pop();
                $header = $zmsg->pop();
                assert($header == MDPW_WORKER);
                
                $command = $zmsg->pop();
                if($command == MDPW_REQUEST) {
                    //  We should pop and save as many addresses as there are
                    //  up to a null part, but for now, just save one...
                    $this->reply_to = $zmsg->unwrap();
                    return $zmsg;//  We have a request to process
                } else if($command == MDPW_HEARTBEAT) {
                    // Do nothing for heartbeats
                } else if($command == MDPW_DISCONNECT) {
                    $this->connect_to_broker();
                } else {
                    echo "E: invalid input message", PHP_EOL;
                    echo $zmsg->__toString();
                }
            } else if(--$this->liveness == 0) { // poll ended on timeout, $event being false
                if($this->verbose) {
                    echo "W: disconnected from broker - retrying...", PHP_EOL;
                }
                usleep($this->reconnect*1000);
                $this->connect_to_broker();
            }
            
            // Send HEARTBEAT if it's time
            if(microtime(true) > $this->heartbeat_at) {
                $this->send_to_broker(MDPW_HEARTBEAT, NULL, NULL);
                $this->heartbeat_at = microtime(true) + ($this->heartbeat/1000);
            }
        }
    }
}