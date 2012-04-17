#
#  Weather proxy device
#

package require zmq

zmq context context

# This is where the weather server sits
zmq socket frontend context SUB
frontend connect "tcp://localhost:5556"

# This is our public endpoint for subscribers
zmq socket backend context PUB
backend bind "tcp://*:8100"

# Subscribe on everything
frontend setsockopt SUBSCRIBE ""

# Shunt messages out to our own subscribers
while {1} {
    while {1} {
	# Process all parts of the message
	zmq message msg
	frontend recv_msg msg
	set more [frontend getsockopt RCVMORE]
	backend send_msg msg [expr {$more?{SNDMORE}:{}}]
	msg close
	if {!$more} {
	    break ;# Last message part
	}
    }
}

# We don't actually get here but if we did, we'd shut down neatly
frontend close
backend close
context term
