#
# Task worker
# Connects PULL socket to tcp://localhost:5557
# Collects workloads from ventilator via that socket
# Connects PUSH socket to tcp://localhost:5558
# Sends results to sink via that socket
#

package require zmq

zmq context context

# Socket to receive messages on
zmq socket receiver context PULL
receiver connect "tcp://localhost:5557"

# Socket to send messages to
zmq socket sender context PUSH
sender connect "tcp://localhost:5558"

# Process tasks forever
while {1} {
    set string [receiver recv]
    # Simple progress indicator for the viewer
    puts -nonewline "$string."
    flush stdout
    # Do the work
    after $string
    # Send result to sink
    sender send "$string"
}

receiver close
sender close
context term
