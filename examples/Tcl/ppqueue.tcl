#
#  Paranoid Pirate queue
#

package require zmq

set HEARTBEAT_LIVENESS  3       ;#  3-5 is reasonable
set HEARTBEAT_INTERVAL  1       ;#  secs

#  Paranoid Pirate Protocol constants
set PPP_READY       "READY"     ;#  Signals worker is ready
set PPP_HEARTBEAT   "HEARTBEAT" ;#  Signals worker heartbeat

#  This defines one active worker in our worker list
# dict with keys address, identity and expiry

#  Construct new worker
proc s_worker_new {address} {
    global HEARTBEAT_LIVENESS HEARTBEAT_INTERVAL
    return [dict create address $address identity $address expiry [expr {[clock seconds] + $HEARTBEAT_INTERVAL * $HEARTBEAT_LIVENESS}]]
}

#  Worker is ready, remove if on list and move to end
proc s_worker_ready {self workersnm} {
    upvar $workersnm workers
    set nworkers {}
    foreach worker $workers {
        if {[dict get $self identity] ne [dict get $worker identity]} {
	    lappend nworkers $worker
        }
    }
    lappend nworkers $self
    set workers $nworkers
}

#  Return next available worker address
proc s_workers_next {workersnm} {
    upvar $workersnm workers
    set workers [lassign $workers worker]
    return [dict get $worker address]
}

#  Look for & kill expired workers. Workers are oldest to most recent,
#  so we stop at the first alive worker.
proc s_workers_purge {workersnm} {
    upvar $workersnm workers
    set nworkers {}
    foreach worker $workers {
	if {[clock seconds] < [dict get $worker expiry]} {
            # Worker is alive
	    lappend nworkers $worker
	}
    }
    set workers $nworkers
}

set ctx [zmq context context]
zmq socket frontend $ctx ROUTER
zmq socket backend $ctx ROUTER
frontend bind "tcp://*:5555" ;#  For clients
backend bind "tcp://*:5556";#  For workers

#  List of available workers
set workers {}

#  Send out heartbeats at regular intervals
set heartbeat_at [expr {[clock seconds] + $HEARTBEAT_INTERVAL}]

while {1} {
    if {[llength $workers]} {
	set poll_set [list [list backend [list POLLIN]] [list frontend [list POLLIN]]]
    } else {
	set poll_set [list [list backend [list POLLIN]]]
    }
    set rpoll_set [zmq poll $poll_set $HEARTBEAT_INTERVAL]
    foreach rpoll $rpoll_set {
	switch [lindex $rpoll 0] {
	    backend {
		#  Handle worker activity on backend
		#  Use worker address for LRU routing
		set msg [zmsg recv backend]

		#  Any sign of life from worker means it's ready
		set address [zmsg unwrap msg]
		set worker [s_worker_new $address]
		s_worker_ready $worker workers

		#  Validate control message, or return reply to client
		if {[llength $msg] == 1} {
		    if {[lindex $msg 0] ne $PPP_READY && [lindex $msg 0] ne $PPP_HEARTBEAT} {
			puts "E: invalid message from worker"
			zmsg dump $msg
		    }
		} else {
		    zmsg send frontend $msg
		}
	    }
	    frontend {
		#  Now get next client request, route to next worker
		set msg [zmsg recv frontend]
		set msg [zmsg push $msg [s_workers_next workers]]
		zmsg send backend $msg
	    }
	}
    }

    #  Send heartbeats to idle workers if it's time
    if {[clock seconds] >= $heartbeat_at} {
	puts "I: heartbeat ([llength $workers])"
	foreach worker $workers {
	    backend sendmore [dict get $worker address]
	    backend send $PPP_HEARTBEAT
	}
	set heartbeat_at [expr {[clock seconds] + $HEARTBEAT_INTERVAL}]
    }
    s_workers_purge workers
}

frontend close
backend close
$ctx term
