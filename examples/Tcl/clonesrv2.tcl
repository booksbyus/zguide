#
#  Clone server Model Two
#

lappend auto_path .
package require KVSimple

if {[llength $argv] == 0} {
    set argv "pub"
} elseif {[llength $argv] != 1} {
    puts "Usage: clonesrv2.tcl <pub|upd>"
    exit 1
}

lassign $argv what

set tclsh [info nameofexecutable]
expr srand([pid])

switch -exact -- $what {
    pub {
	#  Prepare our context and publisher socket
	zmq context context
	set pub [zmq socket publisher context PUB]
	$pub bind "tcp://*:5557"
	set upd [zmq socket updates context PAIR]
	$upd bind "ipc://updates.ipc"

	set sequence 0

	#  Start state manager and wait for synchronization signal
	exec $tclsh clonesrv2.tcl upd > upd.log 2>@1 &
	$upd recv

	while {1} {
	    #  Distribute as key-value message
	    set kvmsg [KVSimple new [incr sequence]]
	    $kvmsg set_key [expr {int(rand()*10000)}]
	    $kvmsg set_body [expr {int(rand()*1000000)}]
	    $kvmsg send $pub
	    $kvmsg send $upd
	    puts [$kvmsg dump]
	    after 500
	}

	$pub close
	$upd close
	context term
    }
    upd {
	zmq context context
	set upd [zmq socket updates context PAIR]
	$upd connect "ipc://updates.ipc"
	$upd send "READY"

	set snp [zmq socket snapshot context ROUTER]
	$snp bind "tcp://*:5556"

	set sequence 0 ;# Current snapshot version number

        #  Apply state update from main thread
	proc apply_state_update {upd} {
	    global kvmap sequence
	    set kvmsg [KVSimple new]
	    $kvmsg recv $upd
	    set sequence [$kvmsg sequence]
	    $kvmsg store kvmap
	}

        #  Execute state snapshot request
	proc execute_state_snapshot_request {snp} {
	    global kvmap sequence

	    set identity [$snp recv]

	    #  Request is in second frame of message
	    set request [$snp recv]
	    if {$request ne "ICANHAZ?"} {
		puts "E: bad request, aborting"
		exit 1
	    }
	    #  Send state snapshot to client
            #  For each entry in kvmap, send kvmsg to client
	    foreach {key value} [array get kvmap] {
		#  Send one state snapshot key-value pair to a socket
		#  Hash item data is our kvmsg object, ready to send
		$snp sendmore $identity
		$value send $snp
	    }

            #  Now send END message with sequence number
	    puts "Sending state snapshot=$sequence"
	    $snp sendmore $identity
	    set kvmsg [KVSimple new $sequence]
	    $kvmsg set_key "KTHXBAI"
	    $kvmsg set_body ""
	    $kvmsg send $snp
	    $kvmsg destroy
	}

	$upd readable [list apply_state_update $upd]
	$snp readable [list execute_state_snapshot_request $snp]
	vwait forever

	$upd close
	$snp close
	context term
    }
}
