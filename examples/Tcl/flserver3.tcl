#
#  Freelance server - Model 2
#  Does some work, replies OK, with message sequencing
#

package require zmq

if {[llength $argv] != 1} {
    puts "Usage: flserver3.tcl <endpoint> ?-v?"
    exit 1
}

set connect_endpoint [lindex $argv 0]
set bind_endpoint [regsub {tcp\://[^\:]+} $connect_endpoint "tcp://*"]
set verbose 0

zmq context context
zmq socket server context ROUTER
server setsockopt IDENTITY $connect_endpoint
server bind $bind_endpoint
puts "I: service is ready at $bind_endpoint"

while {1} {
    set request [zmsg recv server]
    if {$verbose} {
	puts "Request:"
	puts [join [zmsg dump $request] \n]
    }
    if {[llength $request] == 0} {
	break
    }

    set address [zmsg pop request]
    set control [zmsg pop request]
    set reply {}
    if {$control eq "PING"} {
	puts "PING"
	set reply [zmsg add $reply "PONG"]
    } else {
	puts "REQUEST $control"
	set reply [zmsg add $reply $control]
	set reply [zmsg add $reply "OK"]
	set reply [zmsg add $reply "payload"]
    }
    set reply [zmsg push $reply $address]
    if {$verbose} {
	puts "Reply:"
	puts [join [zmsg dump $reply] \n]
    }
    zmsg send server $reply
}

server close
context term
