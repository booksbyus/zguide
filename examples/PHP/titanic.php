<?php
/*
 * Titanic service
 * 
 * Implements server side of http://rfc.zeromq.org/spec:9
 * @author Ian Barber <ian(dot)barber(at)gmail(dot)com>
 */
 
include_once "mdwrkapi.php";
include_once "mdcliapi.php";

/*  Return a new UUID as a printable character string */
function s_generate_uuid() {
    $uuid = sprintf('%04x%04x%04x%03x4%04x%04x%04x%04x',
        mt_rand(0, 65535), mt_rand(0, 65535), // 32 bits for "time_low"
        mt_rand(0, 65535), // 16 bits for "time_mid"
        mt_rand(0, 4095),  // 12 bits before the 0100 of (version) 4 for "time_hi_and_version"
        bindec(substr_replace(sprintf('%016b', mt_rand(0, 65535)), '01', 6, 2)),
            // 8 bits, the last two of which (positions 6 and 7) are 01, for "clk_seq_hi_res"
            // (hence, the 2nd hex digit after the 3rd hyphen can only be 1, 5, 9 or d)
            // 8 bits for "clk_seq_low"
        mt_rand(0, 65535), mt_rand(0, 65535), mt_rand(0, 65535) // 48 bits for "node" 
    ); 
    return $uuid;
}

define("TITANIC_DIR", ".titanic");

/**
 * Returns freshly allocated request filename for given UUID
 */
function s_request_filename($uuid) {
    return TITANIC_DIR . "/" . $uuid . ".req";
}


/**
 * Returns freshly allocated reply filename for given UUID
 */
function s_reply_filename($uuid) {
    return TITANIC_DIR . "/" . $uuid . ".rep";
}


/**
 * Titanic request service
 */
function titanic_request($pipe) {
    $worker = new Mdwrk("tcp://localhost:5555", "titanic.request");
    $reply = null;
    
    while(true) {
        //  Get next request from broker
        $request = $worker->recv($reply);
        
        //  Ensure message directory exists
        if(!is_dir(TITANIC_DIR)) {
            mkdir(TITANIC_DIR);
        }
        
        //  Generate UUID and save message to disk
        $uuid = s_generate_uuid();
        $filename = s_request_filename($uuid);
        
        $fh = fopen($filename, "w");
        $request->save($fh);
        fclose($fh);
        
        //  Send UUID through to message queue
        $reply = new Zmsg($pipe);
        $reply->push($uuid);
        $reply->send();
        
        //  Now send UUID back to client
        // - sent in the next loop iteration
        $reply = new Zmsg();
        $reply->push($uuid);
        $reply->push("200");
    }
}

/**
 * Titanic reply service
 */
function titanic_reply() {
    $worker = new Mdwrk( "tcp://localhost:5555", "titanic.reply", false);
    $reply = null;

    while(true) {
        $request = $worker->recv($reply);
        
        $uuid = $request->pop();
        $req_filename = s_request_filename($uuid);
        $rep_filename = s_reply_filename($uuid);
        
        if(file_exists($rep_filename)) {
            $fh = fopen($rep_filename, "r");
            assert($fh);
            $reply = new Zmsg();
            $reply->load($fh);
            $reply->push("200");
            fclose($fh);
        } else {
            $reply = new Zmsg();
            if(file_exists($req_filename)) {
                $reply->push("300"); // Pending
            } else {
                $reply->push("400"); // Unknown
            }
        }
    } 
}

/**
 * Titanic close service
 */
function titanic_close() {
    $worker = new Mdwrk("tcp://localhost:5555", "titanic.close", false);
    $reply = null;
    
    while(true) {
        $request = $worker->recv($reply);
        
        $uuid = $request->pop();
        $req_filename = s_request_filename($uuid);
        $rep_filename = s_reply_filename($uuid);
        
        unlink($req_filename);
        unlink($rep_filename);
        
        $reply = new Zmsg();
        $reply->push("200");
    }
}

/**
 * Attempt to process a single request, return 1 if successful
 *
 * @param Mdcli $client 
 * @param string $uuid 
 */
function s_service_success($client, $uuid) {
    //  Load request message, service will be first frame
    $filename = s_request_filename($uuid);
    $fh = fopen($filename, "r");
    
    //  If the client already closed request, treat as successful
    if(!$fh) {
        return true;
    }
    
    $request = new Zmsg();
    $request->load($fh);
    fclose($fh);
    $service = $request->pop();

    //  Use MMI protocol to check if service is available
    $mmi_request = new Zmsg();
    $mmi_request->push($service);
    $mmi_reply = $client->send("mmi.service", $mmi_request);
    $service_ok = $mmi_reply && $mmi_reply->pop() == "200";
    
    if($service_ok) {
        $reply = $client->send($service, $request);
        $filename = s_reply_filename($uuid);
        $fh = fopen($filename, "w");
        assert($fh);
        $reply->save($fh);
        fclose($fh);
        return true;
    }
    
    return false;
}

$verbose = $_SERVER['argc'] > 1 && $_SERVER['argv'][1] == '-v';

$pid = pcntl_fork();
if($pid == 0) {
    titanic_reply();
    exit();
}

$pid = pcntl_fork(); 
if($pid == 0) {
    titanic_close();
    exit();
}

$pid = pcntl_fork(); 
if($pid == 0) {
    $pipe = new ZMQSocket(new ZMQContext(), ZMQ::SOCKET_PAIR);
    $pipe->connect("ipc://" . sys_get_temp_dir() . "/titanicpipe");
    titanic_request($pipe);
    exit();
}


//  Create MDP client session with short timeout
$client = new Mdcli("tcp://localhost:5555", $verbose);
$client->set_timeout(1000); // 1 sec
$client->set_retries(1);    // only 1 retry

$request_pipe = new ZMQSocket(new ZMQContext(), ZMQ::SOCKET_PAIR);
$request_pipe->bind("ipc://" . sys_get_temp_dir() . "/titanicpipe");
$read = $write = array();
//  Main dispatcher loop
while(true) {
    //  We'll dispatch once per second, if there's no activity
    $poll = new ZMQPoll();
    $poll->add($request_pipe, ZMQ::POLL_IN);
    $events = $poll->poll($read, $write, 1000);
    
    if($events) {
        //  Ensure message directory exists
        if(!is_dir(TITANIC_DIR)) {
            mkdir(TITANIC_DIR);
        }
        
        //  Append UUID to queue, prefixed with '-' for pending
        $msg = new Zmsg($request_pipe);
        $msg->recv();
        $fh = fopen(TITANIC_DIR . "/queue", "a");
        $uuid = $msg->pop();
        fprintf($fh, "-%s\n", $uuid);
        fclose($fh);
    }
    
    //  Brute-force dispatcher
    if(file_exists(TITANIC_DIR . "/queue")) {
        $fh = fopen(TITANIC_DIR . "/queue", "r+");
        while($fh && $entry  = fread($fh, 33)) {
            //  UUID is prefixed with '-' if still waiting
            if($entry[0] == "-") {
                if($verbose) {
                    printf ("I: processing request %s%s", substr($entry, 1), PHP_EOL);
                }
                if(s_service_success($client, substr($entry, 1))) {
                    //  Mark queue entry as processed
                    fseek($fh, -33, SEEK_CUR);
                    fwrite ($fh, "+");
                    fseek($fh, 32, SEEK_CUR);
                }
            }
            //  Skip end of line, LF or CRLF
            if(fgetc($fh) == "\r") {
                fgetc($fh);
            }
        }
        if($fh) {
            fclose($fh);
        }
    }
}
      