#
#  Freelance server - Model 2
#  Does some work, replies OK, with message sequencing
#

package require zmq

if {[llength $argv] != 1} {
    puts "Usage: flserver2.tcl <endpoint>"
    exit 1
}

zmq context context
zmq socket server context REP
server bind [lindex $argv 0]

puts "I: echo service is ready at [lindex $argv 0]"
while {1} {
    set request [zmsg recv server]
    if {[llength $request] == 0} {
	break
    }
    #  Fail nastily if run against wrong client
    if {[llength $request] != 2} {
	error "request with length 2 expected"
    }

    set address [zmsg pop request]

    set reply {}
    set reply [zmsg add $reply $address]
    set reply [zmsg add $reply "OK"]
    zmsg send server $reply
}

server close
context term
