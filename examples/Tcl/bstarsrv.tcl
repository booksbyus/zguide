#
#  Binary Star server
#

package require TclOO
package require zmq

#  Arguments can be either of:
#      -p  primary server, at tcp://localhost:5001
#      -b  backup server, at tcp://localhost:5002
if {[llength $argv] != 1 || [lindex $argv 0] ni {-p -b}} {
    puts "Usage: bstarsrv.tcl <-p|-b>"
    exit 1
}

#  We send state information every this often
#  If peer doesn't respond in two heartbeats, it is 'dead'
set HEARTBEAT 1000          ;#  In msecs

#  States we can be in at any point in time
set STATE(NONE) 0
set STATE(PRIMARY) 1          ;#  Primary, waiting for peer to connect
set STATE(BACKUP) 2           ;#  Backup, waiting for peer to connect
set STATE(ACTIVE) 3           ;#  Active - accepting connections
set STATE(PASSIVE) 4          ;#  Passive - not accepting connections

#  Events, which start with the states our peer can be in
set EVENT(NONE) 0
set EVENT(PRIMARY) 1           ;#  HA peer is pending primary
set EVENT(BACKUP) 2            ;#  HA peer is pending backup
set EVENT(ACTIVE) 3            ;#  HA peer is active
set EVENT(PASSIVE) 4           ;#  HA peer is passive
set EVENT(REQUEST) 5           ;#  Client makes request

#  Our finite state machine
oo::class create BStar {

    variable state event peer_expiry

    constructor {} {
	set state NONE
	set event NONE
	set peer_expiry 0
    }

    destructor {
    }

    method state_machine {} {
	set exception 0
	if {$state eq "PRIMARY"} {
	    #  Primary server is waiting for peer to connect
	    #  Accepts CLIENT_REQUEST events in this state
	    if {$event eq "BACKUP"} {
		puts "I: connected to backup (slave), ready as master"
		set state ACTIVE
	    } elseif {$event eq "ACTIVE"} {
		puts "I: connected to backup (master), ready as slave"
		set state PASSIVE
	    }
	} elseif {$state eq "BACKUP"} {
	    #  Backup server is waiting for peer to connect
	    #  Rejects CLIENT_REQUEST events in this state
	    if {$event eq "ACTIVE"} {
		puts "I: connected to primary (master), ready as slave"
		set state PASSIVE
	    } elseif {$event eq "REQUEST"} {
		set exception 1
	    }
	} elseif {$state eq "ACTIVE"} {
	    #  Server is active
	    #  Accepts CLIENT_REQUEST events in this state
	    if {$event eq "ACTIVE"} {
		#  Two masters would mean split-brain
		puts "E: fatal error - dual masters, aborting"
		set exception 1
	    }
	} elseif {$state eq "PASSIVE"} {
	    #  Server is passive
	    #  CLIENT_REQUEST events can trigger failover if peer looks dead
	    if {$event eq "PRIMARY"} {
		#  Peer is restarting - become active, peer will go passive
		puts "I: primary (slave) is restarting, ready as master"
		set state ACTIVE
	    } elseif {$event eq "BACKUP"} {
		#  Peer is restarting - become active, peer will go passive
		puts "I: backup (slave) is restarting, ready as master"
		set state ACTIVE
	    } elseif {$event eq "PASSIVE"} {
		#  Two passives would mean cluster would be non-responsive
		puts "E: fatal error - dual slaves, aborting"
		set exception 1
	    } elseif {$event eq "REQUEST"} {
		#  Peer becomes master if timeout has passed
		#  It's the client request that triggers the failover
		if {$peer_expiry <= 0} {
		    error "peer_expiry must be > 0"
		}
		if {[clock milliseconds] >= $peer_expiry} {
		    #  If peer is dead, switch to the active state
		    puts "I: failover successful, ready as master"
		    set state ACTIVE
		} else {
		    #  If peer is alive, reject connections
		    set exception 1
		}
	    }
	}
	return $exception
    }

    method set_state {istate} {
	set state $istate
    }

    method set_event {ievent} {
	set event $ievent
    }

    method state {} {
	return $state
    }

    method update_peer_expiry {} {
	set peer_expiry [expr {[clock milliseconds] + 2 * $::HEARTBEAT}]
    }
}

zmq context context
zmq socket statepub context PUB
zmq socket statesub context SUB
statesub setsockopt SUBSCRIBE ""
zmq socket frontend context ROUTER

set fsm [BStar new]

if {[lindex $argv 0] eq "-p"} {
    puts "I: Primary master, waiting for backup (slave)"
    frontend bind "tcp://*:5001"
    statepub bind "tcp://*:5003"
    statesub connect "tcp://localhost:5004"
    $fsm set_state PRIMARY
} elseif {[lindex $argv 0] eq "-b"} {
    puts "I: Backup slave, waiting for primary (master)"
    frontend bind "tcp://*:5002"
    statepub bind "tcp://*:5004"
    statesub connect "tcp://localhost:5003"
    $fsm set_state BACKUP
}

#  Set timer for next outgoing state message
set send_state_at [expr {[clock milliseconds] + $HEARTBEAT}]

while {1} {
    set timeleft [expr {$send_state_at - [clock milliseconds]}]
    if {$timeleft < 0} {
	set timeleft 0
    }
    foreach rpoll [zmq poll {{frontend {POLLIN}} {statesub {POLLIN}}} $timeleft] {
	switch -exact -- [lindex $rpoll 0] {
	    frontend {
		#  Have a client request
		set msg [zmsg recv frontend]
		$fsm set_event REQUEST
		if {[$fsm state_machine] == 0} {
		    zmsg send frontend $msg
		}
	    }
	    statesub {
		#  Have state from our peer, execute as event
		set state [statesub recv]
		$fsm set_event $state
		if {[$fsm state_machine]} {
		    break ;# Error, so exit
		}
		$fsm update_peer_expiry
	    }
	}
    }
    #  If we timed-out, send state to peer
    if {[clock milliseconds] >= $send_state_at} {
	statepub send [$fsm state]
	set send_state_at [expr {[clock milliseconds] + $HEARTBEAT}]
    }
}

statepub close
statesub close
frontend close
context term
