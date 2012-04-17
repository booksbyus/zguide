#
# Clone client Model Two
#

lappend auto_path .
package require KVSimple

#  Prepare our context and subscriber
zmq context context
set snp [zmq socket snapshot context DEALER]
$snp connect "tcp://localhost:5556"
set sub [zmq socket subscriber context SUB]
$sub setsockopt SUBSCRIBE ""
$sub connect "tcp://localhost:5557"

# Get state snapshot
set sequence 0
$snp send "ICANHAZ?"
while {1} {
    set kvmsg [KVSimple new]
    $kvmsg recv $snp
    if {[$kvmsg key] eq "KTHXBAI"} {
	set sequence [$kvmsg sequence]
	puts "Received snapshot=$sequence"
	$kvmsg destroy
	break
    }
    $kvmsg store kvmap
}

#  Now apply pending updates, discard out-of-sequence messages
while {1} {
    set kvmsg [KVSimple new]
    $kvmsg recv $sub
    puts [$kvmsg dump]
    if {[$kvmsg sequence] > $sequence} {
	puts "    store"
	$kvmsg store kvmap
    } else {
	puts "    ignore"
	$kvmsg destroy
    }
}

$snp close
$sub close
context term
