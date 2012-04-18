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

# Process messages from both sockets
# We prioritize traffic from the task ventilator
while {1} {
    # Process any waiting task
    for {set rc 0} {!$rc} {} {
	zmq message task
	if {[set rc [receiver recv_msg task NOBLOCK]] == 0} {
	    # Do the work
	    set string [task data]
	    puts "Process task: $string"
	    after $string
	    # Send result to sink
	    sender send "$string"
	}
	task close
    }
    # Process any waiting weather update
    for {set rc 0} {!$rc} {} {
	zmq message msg
	if {[set rc [subscriber recv_msg msg NOBLOCK]] == 0} {
	    puts "Weather update: [msg data]"
	}
	msg close
    }
    # No activity, sleep for 1 msec
    after 1
}

# We never get here but clean up anyhow
sender close
receiver close
subscriber close
context term
