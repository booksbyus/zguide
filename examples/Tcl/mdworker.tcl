#
#  Majordomo Protocol worker example
#  Uses the mdwrk API to hide all MDP aspects
#

lappend auto_path .
package require MDWorker 1.0

set verbose 0
foreach {k v} $argv {
    if {$k eq "-v"} { set verbose 1 }
}

set session [MDWorker new "tcp://localhost:5555" "echo" $verbose]

set reply {}
while {1} {
    set request [$session recv $reply]
    if {[llength $request] == 0} {
	break ;# Worker was interrupted
    }
    set reply [list "$request @ [clock format [clock seconds]] from $session"] ;#  Echo is complex… :-)
}

$session destroy
