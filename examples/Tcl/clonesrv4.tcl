#
#  Clone server Model Four
#

lappend auto_path .
package require KVSimple

#  Prepare our context and sockets
zmq context context
set snp [zmq socket snapshot context ROUTER]
$snp bind "tcp://*:5556"
set pub [zmq socket publisher context PUB]
$pub bind "tcp://*:5557"
set col [zmq socket collector context PULL]
$col bind "tcp://*:5558"

set sequence 0

#  Apply state update sent from client
proc apply_state_update {col pub} {
    global sequence kvmap
    set kvmsg [KVSimple new]
    $kvmsg recv $col
    $kvmsg set_sequence [incr sequence]
    $kvmsg send $pub
    $kvmsg store kvmap
    puts "I: publishing update $sequence"
}

#  Execute state snapshot request
proc execute_state_snapshot_request {snp} {
    global sequence
    set identity [$snp recv]
    #  Request is in second frame of message
    set request [$snp recv]
    if {$request ne "ICANHAZ?"} {
	puts "E: bad request, aborting"
	exit 1
    }

    set subtree [$snp recv]

    #  Send state snapshot to client
    #  For each entry in kvmap, send kvmsg to client
    foreach {key value} [array get kvmap] {
	#  Send one state snapshot key-value pair to a socket
	#  Hash item data is our kvmsg object, ready to send
	if {[string match $subtree* [$value key]]} {
	    $snp sendmore $identity
	    $value send $snp
	}
    }

    #  Now send END message with sequence number
    puts "I: sending snapshot=$sequence"
    $snp sendmore $identity
    set kvmsg [KVSimple new $sequence]
    $kvmsg set_key "KTHXBAI"
    $kvmsg set_body $subtree
    $kvmsg send $snp
    $kvmsg destroy
}

$col readable [list apply_state_update $col $pub]
$snp readable [list execute_state_snapshot_request $snp]

vwait forever

$col close
$pub close
$snp close
context term
