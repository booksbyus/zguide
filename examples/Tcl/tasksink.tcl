#
# Task sink
# Binds PULL socket to tcp://localhost:5558
# Collects results from workers via that socket
#

package require zmq

# Prepare our context and socket
zmq context context
zmq socket receiver context PULL
receiver bind "tcp://*:5558"

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

receiver close
context term
