#
# Simple request-reply broker
#

package require zmq

# Prepare our context and sockets
zmq context context
zmq socket frontend context ROUTER
zmq socket backend context DEALER
frontend bind "tcp://*:5559"
backend bind "tcp://*:5560"

# Worker procs
proc process_frontend {} {
    while {1} {
	# Process all parts of the message
	zmq message message
	::frontend recv_msg message
	set more [::frontend getsockopt RCVMORE]
	::backend send_msg message [expr {$more?"SNDMORE":""}]
	message close
	if {!$more} {
	    break ; # Last message part
	}
    }
}

proc process_backend {} {
    while {1} {
	# Process all parts of the message
	zmq message message
	::backend recv_msg message
	set more [::backend getsockopt RCVMORE]
	::frontend send_msg message [expr {$more?"SNDMORE":""}]
	message close
	if {!$more} {
	    break ; # Last message part
	}
    }
}

# Switch messages between sockets
frontend readable [list process_frontend]
backend readable [list process_backend]

vwait forever

# We never get here but clean up anyhow
frontend close
backend close
context term
