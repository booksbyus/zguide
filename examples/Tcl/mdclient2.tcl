#
#  Majordomo Protocol client example
#  Uses the mdcli API to hide all MDP aspects
#

lappend auto_path .
package require MDClient 2.0

set verbose 0
foreach {k v} $argv {
    if {$k eq "-v"} { set verbose 1 }
}

set session [MDClient new "tcp://localhost:5555" $verbose]

for {set count 0} {$count < 10000} {incr count} {
    set request [list "Hello world"]
    set reply [$session send "echo" $request]
}

for {set count 0} {$count < 10000} {incr count} {
    set reply [$session recv]
    if {[llength $reply] == 0} {
	break ;#  Interrupt or failure
    }
}

puts "$count requests received"

$session destroy
