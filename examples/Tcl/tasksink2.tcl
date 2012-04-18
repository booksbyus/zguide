#
# Task sink - design 2
# Adds pub-sub flow to send kill signal to workers
#

package require zmq

zmq context context

# Socket to receive messages on
zmq socket receiver context PULL
receiver bind "tcp://*:5558"

# Socket to worker control
zmq socket controller context PUB
controller bind "tcp://*:5559"

# Wait for start of batch
set string [receiver recv]

# Start our clock now
set start_time [clock milliseconds]

# Process 100 confirmations
for {set task_nbr 0} {$task_nbr < 100} {incr task_nbr} {
    set string [receiver recv]
    if {($task_nbr/10)*10 == $task_nbr} {
	puts -nonewline ":"
    } else {
	puts -nonewline "."
    }
    flush stdout
}
# Calculate and report duration of batch
puts "Total elapsed time: [expr {[clock milliseconds]-$start_time}]msec"

controller send "KILL"

receiver close
controller close
context term
