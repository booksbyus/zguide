#
#  Paranoid Pirate worker
#

package require zmq

set HEARTBEAT_LIVENESS  3       ;#  3-5 is reasonable
set HEARTBEAT_INTERVAL  1000    ;#  msecs
set INTERVAL_INIT       1000    ;#  Initial reconnect
set INTERVAL_MAX       32000    ;#  After exponential backoff

#  Paranoid Pirate Protocol constants
set PPP_READY       "READY"      ;#  Signals worker is ready
set PPP_HEARTBEAT   "HEARTBEAT"  ;#  Signals worker heartbeat

expr {srand([pid])}

#  Helper function that returns a new configured socket
#  connected to the Paranoid Pirate queue

proc s_worker_socket {ctx} {
    global PPP_READY

    set worker [zmq socket worker $ctx DEALER]
    $worker connect "tcp://localhost:5556"

    #  Tell queue we're ready for work
    puts "I: worker ready"
    $worker send $PPP_READY

    return $worker
}

set ctx [zmq context context]
set worker [s_worker_socket $ctx]

#  If liveness hits zero, queue is considered disconnected
set liveness $HEARTBEAT_LIVENESS
set interval $INTERVAL_INIT

#  Send out heartbeats at regular intervals
set heartbeat_at [expr {[clock seconds] + $HEARTBEAT_INTERVAL}]

set cycles 0
while {1} {
    set poll_set [list [list $worker [list POLLIN]]]
    set rpoll_set [zmq poll $poll_set $HEARTBEAT_INTERVAL]
    if {[llength $rpoll_set] && "POLLIN" in [lindex $rpoll_set 0 1]} {
	#  Get message
	#  - 3-part envelope + content -> request
	#  - 1-part HEARTBEAT -> heartbeat
	set msg [zmsg recv $worker]

	if {[llength $msg] == 3} {
	    #  Simulate various problems, after a few cycles
	    incr cycles
	    if {$cycles > 3 && [expr {int(rand()*5)}] == 0} {
		puts "I: simulating a crash"
		break
	    } elseif {$cycles > 3 && [expr {int(rand()*5)}] == 0} {
		puts "I: simulating CPU overload"
		after 3000
	    }
	    puts "I: normal reply"
	    zmsg send $worker $msg
	    set liveness $HEARTBEAT_LIVENESS
	    after 1000 ;#  Do some heavy work
	} elseif {[llength $msg] == 1} {
	    if {[lindex $msg 0] eq $PPP_HEARTBEAT} {
		puts "I: heartbeat"
		set liveness $HEARTBEAT_LIVENESS
	    } else {
		puts "E: invalid message"
		zmsg dump $msg
	    }
	} else {
	    puts "E: invalid message"
	    zmsg dump $msg
	}
	set interval $INTERVAL_INIT
    } elseif {[incr liveness -1] == 0} {
	puts "W: heartbeat failure, can't reach queue"
	puts "W: reconnecting in $interval msec..."
	after $interval

	if {$interval < $INTERVAL_MAX} {
	    set interval [expr {$interval * 2}]
	}
	$worker close
	set worker [s_worker_socket $ctx]
	set liveness $HEARTBEAT_LIVENESS
    }
    #  Send heartbeat to queue if it's time
    if {[clock seconds] > $heartbeat_at} {
	set heartbeat_at [expr {[clock seconds] + $HEARTBEAT_INTERVAL}]
	puts "I: worker heartbeat"

	$worker send $PPP_HEARTBEAT
    }
}

$worker close
$ctx term
