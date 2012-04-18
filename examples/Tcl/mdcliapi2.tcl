# Majordomo Protocol Client API, Tcl version.
# Implements the MDP/Worker spec at http:#rfc.zeromq.org/spec:7.

package require TclOO
package require zmq
package require mdp

package provide MDClient 2.0

oo::class create MDClient {

    variable context broker verbose timeout retries client

    constructor {ibroker {iverbose 0}} {
	set context [zmq context mdcli_context_[::mdp::contextid]]
	set broker $ibroker
	set verbose $iverbose
	set timeout 2500
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
	set client [zmq socket mdcli_socket_[::mdp::socketid] $context DEALER]
	$client connect $broker
	if {$verbose} {
	    puts "I: connecting to broker at $broker..."
	}
    }

    method set_timeout {itimeout} {
	set timeout $itimeout
    }

    # Send request to broker
    # Takes ownership of request message and destroys it when sent.
    method send {service request} {
	#  Prefix request with protocol frames
	#  Frame 0: empty (REQ emulation)
	#  Frame 1: "MDPCxy" (six bytes, MDP/Client x.y)
	#  Frame 2: Service name (printable string)
	set request [zmsg push $request $service]
	set request [zmsg push $request $mdp::MDPC_CLIENT]
	set request [zmsg push $request ""]
	if {$verbose} {
	    puts "I: send request to '$service' service:"
	    puts [join [zmsg dump $request] \n]
	}
	zmsg send $client $request
    }

    #  Returns the reply message or NULL if there was no reply. Does not
    #  attempt to recover from a broker failure, this is not possible
    #  without storing all unanswered requests and resending them all...
    method recv {} {
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
	    if {[llength $msg] < 4} {
		error "message size < 4"
	    }
	    set empty [zmsg pop msg]
	    if {[string length $empty]} {
		error "expected empty frame"
	    }
	    set header [zmsg pop msg]
	    if {$header ne $mdp::MDPC_CLIENT} {
		error "unexpected header"
	    }
	    set service [zmsg pop msg]

	    return $msg ;# Success
	}
	if {$verbose} {
	    puts "W: permanent error, abandoning"
	}
	return {}
    }
}
