#
# Reading from multiple sockets
# This version uses a simple recv loop
#

package require zmq

# Prepare our context and sockets
zmq context context

# Connect to task ventilator
zmq socket receiver context PULL
receiver connect "tcp://localhost:5557"

# Connect to weather server
zmq socket subscriber context SUB
subscriber connect "tcp://*:5556"
subscriber setsockopt SUBSCRIBE "10001"

# Socket to send messages to
zmq socket sender context PUSH
sender connect "tcp://localhost:5558"

# Initialise poll set
set poll_set [list [list receiver [list POLLIN]] [list subscriber [list POLLIN]]]

# Process message from both sockets
while {1} {
    set rpoll_set [zmq poll $poll_set -1]
    foreach rpoll $rpoll_set {
	switch [lindex $rpoll 0] {
	    receiver {
		if {"POLLIN" in [lindex $rpoll 1]} {
		    set string [receiver recv]
		    # Do the work
		    puts "Process task: $string"
		    after $string
		    # Send result to sink
		    sender send "$string"
		}
	    }
	    subscriber {
		if {"POLLIN" in [lindex $rpoll 1]} {
		    set string [subscriber recv]
		    puts "Weather update: $string"
		}
	    }
	}
    }
    # No activity, sleep for 1 msec
    after 1
}

# We never get here but clean up anyhow
sender close
receiver close
subscriber close
context term
