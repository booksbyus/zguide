# Majordomo Protocol Worker API, Tcl version.
# Implements the MDP/Worker spec at http://rfc.zeromq.org/spec:7.

package require TclOO
package require zmq
package require mdp

package provide MDWorker 1.0

oo::class create MDWorker {

    variable context broker service worker verbose heartbeat_at liveness heartbeat reconnect expect_reply reply_to

    constructor {ibroker iservice {iverbose}} {
	set context [zmq context mdwrk_context_[::mdp::contextid]]
	set broker $ibroker
	set service $iservice
	set verbose $iverbose
	set heartbeat 2500
	set reconnect 2500
	set expect_reply 0
	set reply_to ""
	set worker ""
	my connect_to_broker
    }

    destructor {
	$worker close
	$context term
    }

    # Send message to broker
    method send_to_broker {command option msg} {
	# Stack protocol envelope to start of message
	if {[string length $option]} {
	    set msg [zmsg push $msg $option]
	}
	set msg [zmsg push $msg $::mdp::MDPW_COMMAND($command)]
	set msg [zmsg push $msg $::mdp::MDPW_WORKER]
	set msg [zmsg push $msg ""]

	if {$verbose} {
	    puts "I: sending $command to broker"
	    puts [join [zmsg dump $msg] \n]
	}

	zmsg send $worker $msg
    }

    # Connect or reconnect to broker
    method connect_to_broker {} {
	if {[string length $worker]} {
	    $worker close
	}
	set worker [zmq socket mdwrk_socket_[::mdp::socketid] $context DEALER]
	$worker connect $broker
	if {$verbose} {
	    puts "I: connecting to broker at $broker..."
	}
	#  Register service with broker
	my send_to_broker READY $service {}
	#  If liveness hits zero, queue is considered disconnected
	set liveness $::mdp::HEARTBEAT_LIVENESS
	set heartbeat_at [expr {[clock milliseconds] + $heartbeat}]
    }

    #  Set heartbeat delay
    method set_heartbeat {iheartbeat} {
	set heartbeat $iheartbeat
    }

    #  Set reconnect delay
    method set_reconnect {ireconnect} {
	set reconnect $ireconnect
    }

    #  Send reply, if any, to broker and wait for next request.
    method recv {reply} {
	#  Format and send the reply if we were provided one
	if {!([string length $reply] || !$expect_reply)} {
	    error "reply expected"
	}
	if {[string length $reply]} {
	    if {![string length $reply_to]} {
		error "no reply_to found"
	    }
	    set reply [zmsg wrap $reply $reply_to]
	    my send_to_broker REPLY {} $reply
	}
	set expect_reply 1

	while {1} {
	    set poll_set [list [list $worker [list POLLIN]]]
	    set rpoll_set [zmq poll $poll_set $heartbeat]
	    if {[llength $rpoll_set] && "POLLIN" in [lindex $rpoll_set 0 1]} {
		set msg [zmsg recv $worker]
		if {$verbose} {
		    puts "I: received message from broker:"
		    puts [join [zmsg dump $msg] \n]
		}
		set liveness $::mdp::HEARTBEAT_LIVENESS

		# Don't try to handle errors, just assert noisily
		if {[llength $msg] < 3} {
		    error "invalid message size"
		}
		set empty [zmsg pop msg]
		if {[string length $empty]} {
		    error "expected empty frame"
		}
		set header [zmsg pop msg]
		if {$header ne $mdp::MDPW_WORKER} {
		    error "unexpected header"
		}

		set command [zmsg pop msg]
		if {$command eq $::mdp::MDPW_COMMAND(REQUEST)} {
		    # We should pop and save as many addresses as there are
		    # up to a null part, but for now, just save one…
		    set reply_to [zmsg unwrap msg]
		    return $msg ;# We have a request to process
		} elseif {$command eq $mdp::MDPW_COMMAND(HEARTBEAT)} {
		     ;# Do nothing for heartbeats
		} elseif {$command eq $mdp::MDPW_COMMAND(DISCONNECT)} {
		    my connect_to_broker
		} else {
		    puts "E: invalid input message"
		    puts [join [zmsg dump $msg] \n]
		}
	    } elseif {[incr liveness -1] == 0} {
		if {$verbose} {
		    puts "W: disconnected from broker - retrying..."
		}
		after $reconnect
		my connect_to_broker
	    }
	    # Send HEARTBEAT if it's time
	    if {[clock milliseconds] > $heartbeat_at} {
		my send_to_broker HEARTBEAT {} {}
		set heartbeat_at [expr {[clock milliseconds] + $heartbeat}]
	    }
	}
    }
}
