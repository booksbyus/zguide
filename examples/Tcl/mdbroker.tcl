#
#  Majordomo Protocol broker
#  A minimal implementation of http://rfc.zeromq.org/spec:7 and spec:8
#

lappend auto_path .
package require TclOO
package require zmq
package require mdp

lappend auto_path .

set verbose 0
foreach {k v} $argv {
    if {$k eq "-v"} { set verbose 1 }
}

oo::class create MDBroker {

    variable ctx socket verbose services workers waiting heartbeat_at endpoint

    constructor {{iverbose 0}} {
	set ctx [zmq context mdbroker_context_[::mdp::contextid]]
	set socket [zmq socket mdbroker_socket_[::mdp::socketid] $ctx ROUTER]
	set verbose $iverbose
	# services -> array
	# workers -> array
	set waiting [list]
	set heartbeat_at [expr {[clock milliseconds] + $::mdp::HEARTBEAT_INTERVAL}]
	set endpoint ""
    }

    destructor {
	foreach {k v} [array get services] { $v destroy }
	foreach {k v} [array get workers] { $v destroy }
	$socket close
	$ctx term
    }

    #  Bind broker to endpoint, can call this multiple times
    #  We use a single socket for both clients and workers.
    method bind {iendpoint} {
	set endpoint $iendpoint
	$socket bind $endpoint
	if {$verbose} {
	    puts "I: MDP broker is active at $endpoint"
	}
    }

    #  Delete any idle workers that haven't pinged us in a while.
    #  We know that workers are ordered from oldest to most recent.
    method purge_workers {} {
	set i 0
	foreach worker $waiting {
	    if {[clock milliseconds] < [$worker expiry]} {
		break  ;#  Worker is alive, we're done here
	    }
	    my worker_delete $worker 0
	    incr i
	}
	set waiting [lrange $waiting $i end]
    }

    #  Send heartbeat request to all workers
    method heartbeat_workers {} {
	foreach worker $waiting {
	    my worker_send $worker HEARTBEAT {} {}
	}
	set heartbeat_at [expr {[clock milliseconds] + $::mdp::HEARTBEAT_INTERVAL}]
    }

    #  Locate or create new service entry
    method service_require {name} {
	if {![info exists services($name)]} {
	    set services($name) [MDBrokerService new $name]
	    if {$verbose} {
		puts "I: added service: $name"
	    }
	}
	return $services($name)
    }

    #  Dispatch requests to waiting workers as possible
    method service_dispatch {service {msg {}}} {
	if {[llength $msg]} {
	    $service add_request $msg
	}
	my purge_workers
	while {[$service serviceable]} {
	    lassign [$service pop_worker_and_request] worker msg
	    set idx [lsearch $waiting $worker]
	    if {$idx >= 0} {
		set waiting [lreplace $waiting $idx $idx]
	    }
	    my worker_send $worker REQUEST {} $msg
	}
    }

    #  Handle internal service according to 8/MMI specification
    method service_internal {service_frame msg} {
	if {$service_frame eq "mmi.service"} {
	    if {[info exists services([lindex $msg end])] && [$services([lindex $msg end]) has_workers]} {
		set return_code 200
	    } else {
		set return_code 404
	    }
	} else {
	    set return_code 501
	}

	lset msg end $return_code

	my rewrap_and_send $msg $service_frame
    }

    #  Creates worker if necessary
    method worker_require {address} {
	set identity [zmq zframe_strhex $address]
	if {![info exists workers($identity)]} {
	    set workers($identity) [MDBrokerWorker new $address $identity]
	    if {$verbose} {
		puts "I: registering new worker: $identity"
	    }
	}
	return $workers($identity)
    }

    #  Deletes worker from all data structures, and destroys worker
    method worker_delete {worker disconnect} {
	if {$disconnect} {
	    my worker_send $worker DISCONNECT {} {}
	}
	if {[$worker has_service]} {
	    $worker remove_from_service
	}
	set idx [lsearch $waiting $worker]
	if {$idx >= 0} {
	    set waiting [lreplace $waiting $idx $idx]
	}
	unset workers([$worker identity])
	$worker destroy
    }

    method rewrap_and_send {msg service_frame} {
	#  Remove & save client return envelope and insert the
	#  protocol header and service name, then rewrap envelope.
	set client [zmsg unwrap msg]
	set msg [zmsg push $msg $service_frame]
	set msg [zmsg push $msg $::mdp::MDPC_CLIENT]
	set msg [zmsg wrap $msg $client]
	zmsg send $socket $msg
    }

    #  Process message sent to us by a worker
    method worker_process {sender msg} {

	if {[llength $msg] < 1} {
	    error "Invalid message, need at least command"
	}

	set command [zmsg pop msg]
	set identity [zmq zframe_strhex $sender]

	set worker_ready [info exists workers($identity)]
	set worker [my worker_require $sender]

	if {$command eq $::mdp::MDPW_COMMAND(READY)} {
	    if {$worker_ready} {
		#  Not first command in session
		my worker_delete $worker 1
	    } elseif {[string match "mmi.*" $sender]} {
		#  Reserved service name
		my worker_delete $worker 1
	    } else {
		#  Attach worker to service and mark as idle
		set service_frame [zmsg pop msg]
		$worker set_service [my service_require $service_frame]
		my worker_waiting $worker
	    }
	} elseif {$command eq $::mdp::MDPW_COMMAND(REPLY)} {
	    if {$worker_ready} {
		my rewrap_and_send $msg [[$worker service] name]
		my worker_waiting $worker
	    } else {
		my worker_delete $worker 1
	    }
	} elseif {$command eq $::mdp::MDPW_COMMAND(HEARTBEAT)} {
	    if {$worker_ready} {
		$worker update_expiry
	    } else {
		my worker_delete $worker 1
	    }
	} elseif {$command eq $::mdp::MDPW_COMMAND(DISCONNECT)} {
	    my worker_delete $worker 0
	} else {
	    puts "E: invalid input message"
	    puts [join [zmsg dump $msg] \n]
	}
    }

    #  Send message to worker
    #  If pointer to message is provided, sends that message. Does not
    #  destroy the message, this is the caller's job.
    method worker_send {worker command option msg} {
	#  Stack protocol envelope to start of message
	if {[string length $option]} {
	    set msg [zmsg push $msg $option]
	}
	set msg [zmsg push $msg $::mdp::MDPW_COMMAND($command)]
	set msg [zmsg push $msg $::mdp::MDPW_WORKER]

	#  Stack routing envelope to start of message
	set msg [zmsg wrap $msg [$worker address]]

	if {$verbose} {
	    puts "I: sending $command to worker"
	    puts [join [zmsg dump $msg] \n]
	}
	zmsg send $socket $msg
    }

    #  This worker is now waiting for work
    method worker_waiting {worker} {
	lappend waiting $worker
	$worker add_to_service
	my service_dispatch [$worker service]
    }

    #  Process a request coming from a client
    method client_process {sender msg} {
	if {[llength $msg] < 2} {
	    error "Invalud message, need name + body"
	}

	set service_frame [zmsg pop msg]
	set service [my service_require $service_frame]

	#  Set reply return address to client sender
	set msg [zmsg wrap $msg $sender]
	if {[string match "mmi.*" $service_frame]} {
	    my service_internal $service_frame $msg
	} else {
	    my service_dispatch $service $msg
	}
    }

    method socket {} {
	return $socket
    }

    method heartbeat_at {} {
	return $heartbeat_at
    }

    method verbose {} {
	return $verbose
    }
}

oo::class create MDBrokerService {

    variable name requests waiting

    constructor {iname} {
	set name $iname
	set requests [list]
	set waiting [list]
    }

    destructor {
    }

    method serviceable {} {
	return [expr {[llength $waiting] && [llength $requests]}]
    }

    method add_request {msg} {
	lappend requests $msg
    }

    method has_workers {} {
	return [llength $waiting]
    }

    method pop_worker_and_request {} {
	set waiting [lassign $waiting worker]
	set requests [lassign $requests msg]
	return [list $worker $msg]
    }

    method add_worker {worker} {
	lappend waiting $worker
    }

    method remove_worker {worker} {
	set idx [lsearch $waiting $worker]
	if {$idx >= 0} {
	    set waiting [lreplace $waiting $idx $idx]
	}
    }

    method name {} {
	return $name
    }
}

oo::class create MDBrokerWorker {

    variable identity address service expiry

    constructor {iaddress iidentity} {
	set address $iaddress
	set identity $iidentity
	set service ""
	set expiry 0
    }

    destructor {
    }

    method has_service {} {
	return [string length $service]
    }

    method service {} {
	return $service
    }

    method expiry {} {
	return $expiry
    }

    method address {} {
	return $address
    }

    method identity {} {
	return $identity
    }

    method set_service {iservice} {
	set service $iservice
    }

    method remove_from_service {} {
	$service remove_worker [self]
	set service ""
    }

    method add_to_service {} {
	$service add_worker [self]
	my update_expiry
    }

    method update_expiry {} {
	set expiry [expr {[clock milliseconds] + $::mdp::HEARTBEAT_EXPIRY}]
    }
}

set broker [MDBroker new $verbose]
$broker bind "tcp://*:5555"

#  Get and process messages forever
while {1} {
    set poll_set [list [list [$broker socket] [list POLLIN]]]
    set rpoll_set [zmq poll $poll_set $::mdp::HEARTBEAT_INTERVAL]

    #  Process next input message, if any
    if {[llength $rpoll_set] && "POLLIN" in [lindex $rpoll_set 0 1]} {
	set msg [zmsg recv [$broker socket]]
	if {[$broker verbose]} {
	    puts "I: received message:"
	    puts [join [zmsg dump $msg] \n]
	}
	set sender [zmsg pop msg]
	set empty [zmsg pop msg]
	set header [zmsg pop msg]

	if {$header eq $::mdp::MDPC_CLIENT} {
	    $broker client_process $sender $msg
	} elseif {$header eq $::mdp::MDPW_WORKER} {
	    $broker worker_process $sender $msg
	} else {
	    puts "E: invalid message:"
	    puts [join [zmsg dump $msg] \n]
	}
    }
    #  Disconnect and delete any expired workers
    #  Send heartbeats to idle workers if needed
    if {[clock milliseconds] > [$broker heartbeat_at]} {
	$broker purge_workers
	$broker heartbeat_workers
    }
}

$broker destroy
