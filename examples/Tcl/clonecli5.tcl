#
# Clone client Model Five
#

lappend auto_path .
package require KVMsg

set SUBTREE "/client/"

#  Prepare our context and subscriber
zmq context context
set snp [zmq socket snapshot context DEALER]
$snp connect "tcp://localhost:5556"
set sub [zmq socket subscriber context SUB]
$sub setsockopt SUBSCRIBE ""
$sub connect "tcp://localhost:5557"
$sub setsockopt SUBSCRIBE $SUBTREE
set pub [zmq socket publisher context PUSH]
$pub connect "tcp://localhost:5558"

expr srand([pid])

# Get state snapshot
set sequence 0
$snp sendmore "ICANHAZ?"
$snp send $SUBTREE
while {1} {
    set kvmsg [KVMsg new]
    $kvmsg recv $snp
    if {[$kvmsg key] eq "KTHXBAI"} {
	set sequence [$kvmsg sequence]
	puts "I: received snapshot=$sequence"
	$kvmsg destroy
	break
    }
    $kvmsg store kvmap
}

proc recv_kvmsg {pub sub} {
    global after_id sequence kvmap alarm
    after cancel $after_id
    $sub readable {}

    set kvmsg [KVMsg new]
    $kvmsg recv $sub

    if {[$kvmsg sequence] > $sequence} {
	set sequence [$kvmsg sequence]
	$kvmsg store kvmap
	puts "I: received update=$sequence"
    } else {
	$kvmsg destroy
    }

    $sub readable [list recv_kvmsg $pub $sub]
    set after_id [after [tickless] [list send_kvmsg $pub $sub]]
}

proc send_kvmsg {pub sub} {
    global after_id sequence kvmap alarm SUBTREE
    $sub readable {}

    set kvmsg [KVMsg new 0]
    $kvmsg set_key $SUBTREE[expr {int(rand()*10000)}]
    $kvmsg set_body [expr {int(rand()*1000000)}]
    $kvmsg set_prop "ttl" [expr {int(rand()*1000000)}]
    $kvmsg send $pub
    $kvmsg destroy
    set alarm [expr {[clock milliseconds] + 1000}]

    $sub readable [list recv_kvmsg $pub $sub]
    set after_id [after [tickless] [list send_kvmsg $pub $sub]]
}

proc tickless {} {
    global alarm
    set t [expr {[clock milliseconds] - $alarm}]
    if {$t < 0} {
	set t 0
    }
    return $t
}

set alarm [expr {[clock milliseconds] + 1000}]
$sub readable [list recv_kvmsg $pub $sub]
set after_id [after [tickless] [list send_kvmsg $pub $sub]]

vwait forever

$pub close
$sub close
$snp close
context term
