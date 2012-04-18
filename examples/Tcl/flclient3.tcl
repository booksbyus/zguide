#
#  Freelance client - Model 3
#  Uses flcliapi class to encapsulate Freelance pattern
#

lappend auto_path .
package require FLClient

#  Create new freelance client object
set client [FLClient new]

#  Connect to several endpoints
puts connect
$client connect "tcp://localhost:5555"
$client connect "tcp://localhost:5556"
$client connect "tcp://localhost:5557"

set requests 100
set start [clock microseconds]
for {set i 0} {$i < $requests} {incr i} {
    puts "request $i --------------------------------------------------"
    set request {}
    set request [zmsg add $request "random name"]
    set reply [$client request $request]
    puts "reply $i --------------------------------------------------"
    if {[llength $reply] == 0} {
	puts "E: name service not available, aborting"
	break
    } else {
	puts [join [zmsg dump $reply] \n]
    }
}
puts "Average round trip cost: [expr {([clock microseconds] - $start) / $requests}] usec"

$client destroy

