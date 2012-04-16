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

# Initialize poll set
set poll_set [list [list frontend [list POLLIN]] [list backend [list POLLIN]]]

# Switch messages between sockets
while {1} {
    set rpoll_set [zmq poll $poll_set -1]
    foreach rpoll $rpoll_set {
	switch [lindex $rpoll 0] {
	    frontend {
		if {"POLLIN" in [lindex $rpoll 1]} {
		    while {1} {
			# Process all parts of the message
			zmq message message
			frontend recv_msg message
			set more [frontend getsockopt RCVMORE]
			backend send_msg message [expr {$more?"SNDMORE":""}]
			message close
			if {!$more} {
			    break ; # Last message part
			}
		    }
		}
	    }
	    backend {
		if {"POLLIN" in [lindex $rpoll 1]} {
		    while {1} {
			# Process all parts of the message
			zmq message message
			backend recv_msg message
			set more [backend getsockopt RCVMORE]
			frontend send_msg message [expr {$more?"SNDMORE":""}]
			message close
			if {!$more} {
			    break ; # Last message part
			}
		    }
		}
	    }
	}
    }
}

# We never get here but clean up anyhow
frontend close
backend close
context term
