# =====================================================================
# bstar - Binary Star reactor

# ---------------------------------------------------------------------
# Copyright (c) 1991-2011 iMatix Corporation <www.imatix.com>
# Copyright other contributors as noted in the AUTHORS file.

# Tcl port by Jos Decoster <jos.decoster@gmail.com>

# This file is part of the ZeroMQ Guide: http://zguide.zeromq.org

# This is free software; you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.

# This software is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Lesser General Public License for more details.

# You should have received a copy of the GNU Lesser General Public
# License along with this program. If not, see
# <http://www.gnu.org/licenses/>.
# =====================================================================

package require TclOO
package require mdp
package require zmq

package provide BStar 1.0

#  We send state information every this often
#  If peer doesn't respond in two heartbeats, it is 'dead'
set BSTAR_HEARTBEAT 1000 ;#  In msecs

#  States we can be in at any point in time
#  STATE(NONE) 0
#  STATE(PRIMARY) 1          ;#  Primary, waiting for peer to connect
#  STATE(BACKUP) 2           ;#  Backup, waiting for peer to connect
#  STATE(ACTIVE) 3           ;#  Active - accepting connections
#  STATE(PASSIVE) 4          ;#  Passive - not accepting connections

#  Events, which start with the states our peer can be in
#  EVENT(NONE) 0
#  EVENT(PRIMARY) 1           ;#  HA peer is pending primary
#  EVENT(BACKUP) 2            ;#  HA peer is pending backup
#  EVENT(ACTIVE) 3            ;#  HA peer is active
#  EVENT(PASSIVE) 4           ;#  HA peer is passive
#  EVENT(REQUEST) 5           ;#  Client makes request

oo::class create BStar {

    variable verbose ctx statepub statesub voter state event peer_expiry voterfn masterfn slavefn

    constructor {istate local remote iverbose} {
	#  Initialize the Binary Star
	set verbose $iverbose
	set ctx [zmq context bstar_context_[::mdp::contextid]]
	set state $istate
	set event NONE
	set peer_expiry 0
	set voterfn {}
	set masterfn {}
	set slavefn {}
	#  Create publisher for state going to peer
	set statepub [zmq socket bstar_socket_[::mdp::socketid] $ctx PUB]
	$statepub bind $local
	#  Create subscriber for state coming from peer
	set statesub [zmq socket bstar_socket_[::mdp::socketid] $ctx SUB]
	$statesub setsockopt SUBSCRIBE ""
	$statesub connect $remote
    }

    destructor {
	$statesub close
	$statepub close
	$ctx term
    }

    method voter_callback {} {
	if {[llength $voterfn]} {
	    {*}$voterfn $voter
	}
    }

    method master_callback {} {
	if {[llength $masterfn]} {
	    {*}$masterfn
	}
    }

    method slave_callback {} {
	if {[llength $slavefn]} {
	    {*}$slavefn
	}
    }

    method log {msg} {
	if {$verbose} {
	    puts "[clock format [clock seconds]] $msg"
	}
    }

    method execute_fsm {} {
	set rc 0
	if {$state eq "PRIMARY"} {
	    #  Primary server is waiting for peer to connect
	    #  Accepts CLIENT_REQUEST events in this state
	    if {$event eq "BACKUP"} {
		my log "I: connected to backup (slave), ready as master"
		set state ACTIVE
		my master_callback
	    } elseif {$event eq "ACTIVE"} {
		my log "I: connected to backup (master), ready as slave"
		set state PASSIVE
		my slave_callback
	    } elseif {$event eq "REQUEST"} {
		# Allow client requests to turn us into the master if we've
		# waited sufficiently long to believe the backup is not
		# currently acting as master (i.e., after a failover)
		if {$peer_expiry <= 0} {
		    error "expecte peer_expiry > 0"
		}
		if {[clock milliseconds] >= $peer_expiry} {
		    my log "I: request from client, ready as master"
		    set state ACTIVE
		    my master_callback
		} else {
		    # Don't respond to clients yet - it's possible we're
		    # performing a failback and the backup is currently master
		    set rc -1
		}
	    }
	} elseif {$state eq "BACKUP"} {
	    #  Backup server is waiting for peer to connect
	    #  Rejects CLIENT_REQUEST events in this state
	    if {$event eq "ACTIVE"} {
		my log "I: connected to primary (master), ready as slave"
		set state PASSIVE
		my slave_callback
	    } elseif {$event eq "REQUEST"} {
		set rc -1
	    }
	} elseif {$state eq "ACTIVE"} {
	    #  Server is active
	    #  Accepts CLIENT_REQUEST events in this state
	    #  The only way out of ACTIVE is death
	    if {$event eq "ACTIVE"} {
		my log "E: fatal error - dual masters, aborting"
		set rc -1
	    }
	} elseif {$state eq "PASSIVE"} {
	    #  Server is passive
	    #  CLIENT_REQUEST events can trigger failover if peer looks dead
	    if {$event eq "PRIMARY"} {
		#  Peer is restarting - become active, peer will go passive
		my log "I: primary (slave) is restarting, ready as master"
		set state ACTIVE
	    } elseif {$event eq "BACKUP"} {
		#  Peer is restarting - become active, peer will go passive
		my log "I: backup (slave) is restarting, ready as master"
		set state ACTIVE
	    } elseif {$event eq "PASSIVE"} {
		#  Two passives would mean cluster would be non-responsive
		my log "E: fatal error - dual slaves, aborting"
		set rc -1
	    } elseif {$event eq "REQUEST"} {
		#  Peer becomes master if timeout has passed
		#  It's the client request that triggers the failover
		if {$peer_expiry < 0} {
		    error "expecte peer_expiry >= 0"
		}
		if {[clock milliseconds] >= $peer_expiry} {
		    #  If peer is dead, switch to the active state
		    my log "I: failover successful, ready as master"
		    set state ACTIVE
		} else {
		    #  If peer is alive, reject connections
		    set rc -1
		}
	    }
	    if {$state eq "ACTIVE"} {
		my master_callback
	    }
	}
	return $rc
    }

    method update_peer_expiry {} {
	set peer_expiry [expr {[clock milliseconds] + 2 * $::BSTAR_HEARTBEAT}]
    }

    #  Reactor event handlers...

    #  Publish our state to peer
    method send_state {} {
	my log "I: send state $state to peer"
	$statepub send $state
	after $::BSTAR_HEARTBEAT [list [self] send_state]
    }

    #  Receive state from peer, execute finite state machine
    method recv_state {} {
	set nstate [$statesub recv]
	my log "I: got state $nstate from peer"
	set event $nstate
	my update_peer_expiry
	my execute_fsm
    }

    #  Application wants to speak to us, see if it's possible
    method voter_ready {} {
	#  If server can accept input now, call appl handler
	set event REQUEST
	if {[my execute_fsm] == 0} {
	    puts "CLIENT REQUEST"
	    my voter_callback
	} else {
	    #  Destroy waiting message, no-one to read it
	    zmsg recv $voter
	}
    }

    #  Create socket, bind to local endpoint, and register as reader for
    #  voting. The socket will only be available if the Binary Star state
    #  machine allows it. Input on the socket will act as a "vote" in the
    #  Binary Star scheme.  We require exactly one voter per bstar instance.
    method voter {endpoint type handler} {
	#  Hold actual handler+arg so we can call this later
	set voter [zmq socket bstar_socket_[::mdp::socketid] $ctx $type]
	$voter bind $endpoint
	set voterfn $handler
    }

    #  Register state change handlers
    method new_master {handler} {
	set masterfn $handler
    }

    method new_slave {handler} {
	set slavefn $handler
    }

    #  Enable/disable verbose tracing
    method set_verbose {iverbose} {
	set verbose $iverbose
    }

    #  Start the reactor, ends if a callback function returns -1
    method start {} {
	my update_peer_expiry
	#  Set-up reactor events
	$statesub readable [list [self] recv_state]
	$voter readable [list [self] voter_ready]
	after $::BSTAR_HEARTBEAT [list [self] send_state]
    }
}
