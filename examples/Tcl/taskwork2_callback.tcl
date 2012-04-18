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

# Functions to process messages
proc work {s} {
    set string [$s recv]
    # Simple progress indicator for the viewer
    puts -nonewline "$string."
    flush stdout
    # Do the work
    after $string
    # Send result to sink
    ::sender send "$string"
}

proc done {s} {
    puts ""
    ::receiver close
    ::sender close
    ::controller close
    ::context term
    exit
}

proc do_something {} {
    puts [clock format [clock seconds]]
    after 1000 do_something
}

# Register callbacks
receiver readable [list work ::receiver]
controller readable [list done ::controller]

do_something

vwait forever

