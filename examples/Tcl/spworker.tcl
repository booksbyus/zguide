#
#  Simple Pirate worker
#  Connects REQ socket to tcp://*:5556
#  Implements worker part of LRU queueing
#

package require zmq

set LRU_READY "READY" ;#  Signals worker is ready
expr {srand([pid])}

zmq context context
zmq socket worker context REQ

#  Set random identity to make tracing easier
set identity [format "%04X-%04X" [expr {int(rand()*0x10000)}] [expr {int(rand()*0x10000)}]]
worker setsockopt IDENTITY $identity
worker connect "tcp://localhost:5556"

#  Tell broker we're ready for work
puts "I: ($identity) worker ready"
worker send $LRU_READY

set cycles 0
while {1} {
    set msg [zmsg recv worker]

    #  Simulate various problems, after a few cycles
    incr cycles
    if {$cycles > 3 && [expr {int(rand()*5)}] == 0} {
	puts "I: ($identity) simulating a crash"
	break
    } elseif {$cycles > 3 && [expr {int(rand()*5)}] == 0} {
	puts "I: ($identity) simulating CPU overload"
	after 3000
    }
    puts "I: ($identity) normal reply"
    after 1000 ;#  Do some heavy work
    zmsg send worker $msg
}

worker close
context term

