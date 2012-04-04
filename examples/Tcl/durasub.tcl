#
#  Durable subscriber
#

package require zmq

zmq context context 1

# Connect our subscriber socket
zmq socket subscriber context SUB
subscriber setsockopt IDENTITY "Hello"
subscriber setsockopt SUBSCRIBE ""
subscriber connect "tcp://localhost:5565"

# Synchronize with publisher
zmq socket sync context PUSH
sync connect "tcp://localhost:5564"
sync send ""

# Get updates, exit when told to do so
while {1} {
    set string [subscriber recv]
    puts $string
    if {$string eq "END"} {
	break;
    }
}

sync close
subscriber close
context term
