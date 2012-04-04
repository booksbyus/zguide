#
#  Binary Star server, using bstar reactor
#

lappend auto_path .
package require BStar

#  Echo service
proc echo {s} {
    set msg [zmsg recv $s]
    zmsg send $s $msg
    return 0
}

#  Arguments can be either of:
#      -p  primary server, at tcp://localhost:5001
#      -b  backup server, at tcp://localhost:5002

if {[lindex $argv 0] eq "-p"} {
    set bstar [BStar new PRIMARY "tcp://*:5003" "tcp://localhost:5004" [expr {[lindex $argv 1] eq "-v"}]]
    $bstar voter "tcp://*:5001" ROUTER echo
} elseif {[lindex $argv 0] eq "-b"} {
    set bstar [BStar new BACKUP "tcp://*:5004" "tcp://localhost:5003" [expr {[lindex $argv 1] eq "-v"}]]
    $bstar voter "tcp://*:5002" ROUTER echo
} else {
    puts "Usage: bstarsrv2.tcl <-p|-b> ?-v?"
    exit 1
}

$bstar start
vwait forever
$bstar destroy
