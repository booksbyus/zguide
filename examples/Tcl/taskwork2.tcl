#
# Task worker - design 2
#  Adds pub-sub flow to receive and respond to kill signal
#

package require zmq

zmq context context

# Socket to receive messages on
zmq socket receiver context PULL
receiver connect "tcp://localhost:5557"

# Socket to send messages to
zmq socket sender context PUSH
sender connect "tcp://localhost:5558"

# Socket for control input
zmq socket controller context SUB
controller connect "tcp://localhost:5559"
controller setsockopt SUBSCRIBE ""

# Process messages from receiver and controller
set poll_set [list [list receiver [list POLLIN]] [list controller [list POLLIN]]]

# Process tasks forever
set poll 1
while {$poll} {
    set rpoll_set [zmq poll $poll_set -1]
    foreach rpoll $rpoll_set {
	switch [lindex $rpoll 0] {
	    receiver {
		if {"POLLIN" in [lindex $rpoll 1]} {
		    set string [receiver recv]
		    # Simple progress indicator for the viewer
		    puts -nonewline "$string."
		    flush stdout
		    # Do the work
		    after $string
		    # Send result to sink
		    sender send "$string"
		}
	    }
	    controller {
		if {"POLLIN" in [lindex $rpoll 1]} {
		    puts ""
		    set poll 0
		}
	    }
	}
    }
}

receiver close
sender close
controller close
context term
