<?php
/*
 * Majordomo Protocol broker
 * A minimal implementation of http://rfc.zeromq.org/spec:7 and spec:8
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */
include_once 'zmsg.php';
include_once 'mdp.php';

//  We'd normally pull these from config data
define("HEARTBEAT_LIVENESS", 3);    //  3-5 is reasonable
define("HEARTBEAT_INTERVAL", 2500); //  msecs
define("HEARTBEAT_EXPIRY", HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS);

/* Main broker work happens here */
$verbose = $_SERVER['argc'] > 1 && $_SERVER['argv'][1] == '-v';
$broker = new Mdbroker($verbose);
$broker->bind("tcp://*:5555");
$broker->listen();

class mdbroker
{
    private $ctx;               //  Our context
    private $socket;            //  Socket for clients & workers
    private $endpoint;          //  Broker binds to this endpoint

    private $services = array();          //  Hash of known services
    private $workers = array();           //  Hash of known workers
    private $waiting = array();           //  List of waiting workers

    private $verbose = false;   //  Print activity to stdout

    //  Heartbeat management
    private $heartbeat_at;  //  When to send HEARTBEAT

    /**
     * Constructor
     *
     * @param boolean $verbose
     */
    public function __construct($verbose = false)
    {
        $this->ctx = new ZMQContext();
        $this->socket = new ZMQSocket($this->ctx, ZMQ::SOCKET_ROUTER);
        $this->verbose = $verbose;
        $this->heartbeat_at = microtime(true) + (HEARTBEAT_INTERVAL/1000);
    }

    /**
     * Bind broker to endpoint, can call this multiple time
     * We use a single socket for both clients and workers.
     *
     * @param string $endpoint
     */
    public function bind($endpoint)
    {
        $this->socket->bind($endpoint);
        if ($this->verbose) {
            printf("I: MDP broker/0.1.1 is active at %s %s", $endpoint, PHP_EOL);
        }
    }

    /**
     * This is the main listen and process loop
     */
    public function listen()
    {
        $read = $write = array();

        //  Get and process messages forever or until interrupted
        while (true) {
            $poll = new ZMQPoll();
            $poll->add($this->socket, ZMQ::POLL_IN);

            $events = $poll->poll($read, $write, HEARTBEAT_INTERVAL);

            //  Process next input message, if any
            if ($events) {
                $zmsg = new Zmsg($this->socket);
                $zmsg->recv();
                if ($this->verbose) {
                    echo "I: received message:", PHP_EOL, $zmsg->__toString();
                }

                $sender = $zmsg->pop();
                $empty = $zmsg->pop();
                $header = $zmsg->pop();

                if ($header == MDPC_CLIENT) {
                    $this->client_process($sender, $zmsg);
                } elseif ($header == MDPW_WORKER) {
                    $this->worker_process($sender, $zmsg);
                } else {
                    echo "E: invalid message", PHP_EOL, $zmsg->__toString();
                }
            }

            //  Disconnect and delete any expired workers
            //  Send heartbeats to idle workers if needed
            if (microtime(true) > $this->heartbeat_at) {
                $this->purge_workers();
                foreach ($this->workers as $worker) {
                    $this->worker_send($worker, MDPW_HEARTBEAT, NULL, NULL);
                }
                $this->heartbeat_at = microtime(true) + (HEARTBEAT_INTERVAL/1000);
            }
        }
    }

    /**
     * Delete any idle workers that haven't pinged us in a while.
     * We know that workers are ordered from oldest to most recent.
     */
    public function purge_workers()
    {
        foreach ($this->waiting as $id => $worker) {
            if (microtime(true) < $worker->expiry) {
                break;      //  Worker is alive, we're done here
            }
            if ($this->verbose) {
                printf("I: deleting expired worker: %s %s",
                    $worker->identity, PHP_EOL);
            }
            $this->worker_delete($worker);
        }
    }

    /**
     * Locate or create new service entry
     *
     * @param  string   $name
     * @return stdClass
     */
    public function service_require($name)
    {
        $service = isset($this->services[$name]) ? $this->services[$name] : NULL;
        if ($service == NULL) {
            $service = new stdClass();
            $service->name = $name;
            $service->requests = array();
            $service->waiting = array();
            $this->services[$name] = $service;
        }

        return $service;
    }

    /**
     * Dispatch requests to waiting workers as possible
     *
     * @param type $service
     * @param type $msg
     */
    public function service_dispatch($service, $msg)
    {
        if ($msg) {
            $service->requests[] = $msg;
        }

        $this->purge_workers();

        while (count($service->waiting) && count($service->requests)) {
            $worker = array_shift($service->waiting);
            $msg = array_shift($service->requests);
            $this->worker_send($worker, MDPW_REQUEST, NULL, $msg);
        }
    }

    /**
     * Handle internal service according to 8/MMI specification
     *
     * @param string $frame
     * @param Zmsg   $msg
     */
    public function service_internal($frame, $msg)
    {
        if ($frame == "mmi.service") {
            $name = $msg->last();
            $service = $this->services[$name];
            $return_code = $service && $service->workers ? "200" : "404";
        } else {
            $return_code = "501";
        }

        $msg->set_last($return_code);

        //  Remove & save client return envelope and insert the
        //  protocol header and service name, then rewrap envelope
        $client = $msg->unwrap();
        $msg->push($frame);
        $msg->push(MDPC_CLIENT);
        $msg->wrap($client, "");
        $msg->set_socket($this->socket)->send();
    }

    /**
     * Creates worker if necessary
     *
     * @param  string   $address
     * @return stdClass
     */
    public function worker_require($address)
    {
        $worker = isset($this->workers[$address]) ? $this->workers[$address] : NULL;
        if ($worker == NULL) {
            $worker = new stdClass();
            $worker->identity = $address;
            $worker->address = $address;
            if ($this->verbose) {
                printf("I: registering new worker: %s %s", $address, PHP_EOL);
            }
            $this->workers[$address] = $worker;
        }

        return $worker;
    }

    /**
     * Remove a worker
     *
     * @param stdClass $worker
     * @param boolean  $disconnect
     */
    public function worker_delete($worker, $disconnect = false)
    {
        if ($disconnect) {
            $this->worker_send($worker, MDPW_DISCONNECT, NULL, NULL);
        }

        if (isset($worker->service)) {
            worker_remove_from_array($worker, $worker->service->waiting);
            $worker->service->workers--;
        }
        worker_remove_from_array($worker, $this->waiting);
        unset($this->workers[$worker->identity]);
    }

    private function worker_remove_from_array($worker, &$array)
    {
        $index = array_search($worker, $array);
        if ($index !== false) {
            unset($array[$index]);
        }
    }

    /**
     * Process message sent to us by a worker
     *
     * @param string $sender
     * @param Zmsg   $msg
     */
    public function worker_process($sender, $msg)
    {
        $command = $msg->pop();
        $worker_ready = isset($this->workers[$sender]);
        $worker = $this->worker_require($sender);
        if ($command == MDPW_READY) {
            if ($worker_ready) {
                $this->worker_delete($worker, true); //  Not first command in session
            } else if(strlen($sender) >= 4      // Reserved service name
                    && substr($sender, 0, 4) == 'mmi.') {
                $this->worker_delete($worker, true);
            } else {
                //  Attach worker to service and mark as idle
                $service_frame = $msg->pop();
                $worker->service = $this->service_require($service_frame);
                $worker->service->workers++;
                $this->worker_waiting($worker);
            }
        } elseif ($command == MDPW_REPLY) {
            if ($worker_ready) {
                //  Remove & save client return envelope and insert the
                //  protocol header and service name, then rewrap envelope.
                $client = $msg->unwrap();
                $msg->push($worker->service->name);
                $msg->push(MDPC_CLIENT);
                $msg->wrap($client, "");
                $msg->set_socket($this->socket)->send();
                $this->worker_waiting($worker);
            } else {
                $this->worker_delete($worker, true);
            }
        } elseif ($command == MDPW_HEARTBEAT) {
            if ($worker_ready) {
                $worker->expiry = microtime(true) + (HEARTBEAT_EXPIRY/1000);
            } else {
                $this->worker_delete($worker, true);
            }
        } elseif ($command == MDPW_DISCONNECT) {
            $this->worker_delete($worker, true);
        } else {
            echo "E: invalid input message", PHP_EOL, $msg->__toString();
        }
    }

    /**
     * Send message to worker
     *
     * @param stdClass $worker
     * @param string   $command
     * @param mixed    $option
     * @param Zmsg     $msg
     */
    public function worker_send($worker, $command, $option, $msg)
    {
        $msg = $msg ? $msg : new Zmsg();
        //  Stack protocol envelope to start of message
        if ($option) {
            $msg->push($option);
        }
        $msg->push($command);
        $msg->push(MDPW_WORKER);

        //  Stack routing envelope to start of message
        $msg->wrap($worker->address, "");

        if ($this->verbose) {
            printf("I: sending %s to worker %s",
               $command, PHP_EOL);
            echo $msg->__toString();
        }

        $msg->set_socket($this->socket)->send();
    }

    /**
     * This worker is now waiting for work
     *
     * @param stdClass $worker
     */
    public function worker_waiting($worker)
    {
        //  Queue to broker and service waiting lists
        $this->waiting[] = $worker;
        $worker->service->waiting[] = $worker;
        $worker->expiry = microtime(true) + (HEARTBEAT_EXPIRY/1000);
        $this->service_dispatch($worker->service, NULL);
    }

    /**
     * Process a request coming from a client
     *
     * @param string $sender
     * @param Zmsg   $msg
     */
    public function client_process($sender, $msg)
    {
        $service_frame = $msg->pop();
        $service = $this->service_require($service_frame);

        //  Set reply return address to client sender
        $msg->wrap($sender, "");
        if (substr($service_frame, 0, 4) == 'mmi.') {
            $this->service_internal($service_frame, $msg);
        } else {
            $this->service_dispatch($service, $msg);
        }
    }
}
