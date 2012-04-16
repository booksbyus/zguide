# Majordomo Protocol Client API, Tcl version.
# Implements the MDP/Worker spec at http:#rfc.zeromq.org/spec:7.

package require TclOO
package require zmq
package require mdp

package provide MDClient 1.0

oo::class create MDClient {

    variable context broker verbose timeout retries client

    constructor {ibroker {iverbose 0}} {
	set context [zmq context mdcli_context_[::mdp::contextid]]
	set broker $ibroker
	set verbose $iverbose
	set timeout 2500
	set retries 3
	set client ""
	my connect_to_broker
    }

    destructor {
	$client close
	$context term
    }

    method connect_to_broker {} {
	if {[string length $client]} {
	    $client close
	}
	set client [zmq socket mdcli_socket_[::mdp::socketid] $context REQ]
	$client connect $broker
	if {$verbose} {
	    puts "I: connecting to broker at $broker..."
	}
    }

    method set_timeout {itimeout} {
	set timeout $itimeout
    }

    method set_retries {iretries} {
	set retries $iretries
    }

    # Send request to broker and get reply by hook or crook
    # Takes ownership of request message and destroys it when sent.
    # Returns the reply message or NULL if there was no reply.
    method send {service request} {
	#  Prefix request with protocol frames
	#  Frame 1: "MDPCxy" (six bytes, MDP/Client x.y)
	#  Frame 2: Service name (printable string)
	set request [zmsg push $request $service]
	set request [zmsg push $request $mdp::MDPC_CLIENT]
	if {$verbose} {
	    puts "I: send request to '$service' service:"
	    puts [join [zmsg dump $request] \n]
	}

	set retries_left $retries
	while {$retries_left} {
	    set msg $request
	    zmsg send $client $msg

	    # Poll socket for a reply, with timeout
	    set poll_set [list [list $client [list POLLIN]]]
	    set rpoll_set [zmq poll $poll_set $timeout]
	    # If we got a reply, process it
	    if {[llength $rpoll_set] && "POLLIN" in [lindex $rpoll_set 0 1]} {
		set msg [zmsg recv $client]
		if {$verbose} {
		    puts "I: received reply:"
		    puts [join [zmsg dump $msg] \n]
		}
		# Don't try to handle errors, just assert noisily
		if {[llength $msg] < 3} {
		    error "message size < 3"
		}
		set header [zmsg pop msg]
		if {$header ne $mdp::MDPC_CLIENT} {
		    error "unexpected header"
		}
		set reply_service [zmsg pop msg]
		if {$reply_service ne $service} {
		    error "unexpected service"
		}
		return $msg
	    } elseif {[incr retries_left -1]} {
		if {$verbose} {
		    puts "W: no reply, reconnecting..."
		}
		# Reconnect socket
		my connect_to_broker
	     } else {
		 if {$verbose} {
		     puts "W: permanent error, abandoning"
		 }
		 break ;# Give up
	     }
	}
	return {}
    }
}
