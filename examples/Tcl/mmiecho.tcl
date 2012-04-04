#
#  MMI echo query example
#

lappend auto_path .
package require MDClient 1.0

set verbose 0
foreach {k v} $argv {
    if {$k eq "-v"} { set verbose 1 }
}

set session [MDClient new "tcp://localhost:5555" $verbose]

foreach service {echo nonexisting} {

    set reply [$session send "mmi.service" $service]

    if {[llength $reply]} {
	puts "Lookup '$service' service: [lindex $reply 0]"
    } else {
	puts "E: no response from broker, make sure it's running"
	break
    }
}

$session destroy
