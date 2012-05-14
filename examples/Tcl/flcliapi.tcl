# =====================================================================
# flcliapi - Freelance Pattern agent class
# Model 3: uses ROUTER socket to address specific services

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
package require zmq
package require mdp

package provide FLClient 1.0

#  If no server replies within this time, abandon request
set GLOBAL_TIMEOUT 3000 ;# msecs
#  PING interval for servers we think are alive
set PING_INTERVAL 2000 ;# msecs
#  Server considered dead if silent for this long
set SERVER_TTL 6000 ;# msecs

oo::class create FLClient {

    variable ctx pipe pipe_address readable agent

    constructor {} {
	set ctx [zmq context flcli_context_[::mdp::contextid]]
	set pipe [zmq socket flcli_pipe_[::mdp::socketid] $ctx PAIR]
	set pipe_address "ipc://flclientpipe_[::mdp::socketid].ipc"
	$pipe connect $pipe_address
	set agent [FLClient_agent new $ctx $pipe_address]
	$agent process
	set readable 0
    }

    destructor {
	$agent destroy
	$pipe setsockopt LINGER 0
	$pipe close
	$ctx term
    }

    #  Sends [CONNECT][endpoint] to the agent
    method connect {endpoint} {
	set msg {}
	set msg [zmsg add $msg "CONNECT"]
	set msg [zmsg add $msg $endpoint]
	zmsg send $pipe $msg
	after 100 ;#  Allow connection to come up
    }

    #  Send & destroy request, get reply
    method is_readable {} {
	set readable 1
    }

    method request {request} {
	set request [zmsg push $request "REQUEST"]
	zmsg send $pipe $request
	$pipe readable [list [self] is_readable]
	vwait [my varname readable]
	$pipe readable {}
	set reply [zmsg recv $pipe]
	if {[llength $reply]} {
	    set status [zmsg pop reply]
	    if {$status eq "FAILED"} {
		set reply {}
	    }
	}
	return $reply
    }
}

oo::class create FLClient_server {

    variable endpoint alive ping_at expires

    constructor {iendpoint} {
	set endpoint $iendpoint
	set alive 0
	set ping_at [expr {[clock milliseconds] + $::PING_INTERVAL}]
	set expires [expr {[clock milliseconds] + $::SERVER_TTL}]
    }

    destructor {
    }

    method ping {socket} {
	if {[clock milliseconds] >= $ping_at} {
	    puts "ping [self]"
	    set ping {}
	    set ping [zmsg add $ping $endpoint]
	    set ping [zmsg add $ping "PING"]
	    zmsg send $socket $ping
	    set ping_at [expr {[clock milliseconds] + $::PING_INTERVAL}]
	}
    }

    method ping_at {} {
	return $ping_at
    }

    method activate {} {
	set oalive $alive
	set alive 1
	set ping_at [expr {[clock milliseconds] + $::PING_INTERVAL}]
	set expires [expr {[clock milliseconds] + $::SERVER_TTL}]
	return $oalive
    }

    method expires {} {
	return $expires
    }

    method endpoint {} {
	return $endpoint
    }

    method no_longer_alive {} {
	set alive 0
    }
}

oo::class create FLClient_agent {

    variable ctx pipe router servers actives sequence request reply expires afterid

    constructor {context pipe_address} {
	set ctx $context
	set pipe [zmq socket flcli_agent_pipe_[::mdp::socketid] $ctx PAIR]
	$pipe bind $pipe_address
	set router [zmq socket flcli_router_[::mdp::socketid] $ctx ROUTER]
	# set servers ;# array
	set actives [list]
	set sequence 0
	set request {}
	set reply {}
	set expires 0
	after 100 ;# Allow connection to come up
    }

    destructor {
	$pipe setsockopt LINGER 0
	$pipe close
	$router setsockopt LINGER 0
	$router close
    }

    method control_message {} {
	puts "control message"
	catch {
	    $pipe readable {}
	    $router readable {}
	    if {[info exists $afterid]} {
		catch {after cancel $afterid}
	    }

	    set msg [zmsg recv $pipe]
	    puts [join [zmsg dump $msg] \n]
	    set command [zmsg pop msg]

	    if {$command eq "CONNECT"} {
		set endpoint [zmsg pop msg]
		puts "I: connecting to $endpoint..."
		$router connect $endpoint
		set server [FLClient_server new $endpoint]
		set servers($endpoint) $server
		lappend actives $server
	    } elseif {$command eq "REQUEST"} {
		#  Prefix request with sequence number and empty envelope
		set request [zmsg push $msg [incr sequence]]
		set expires [expr {[clock milliseconds] + $::GLOBAL_TIMEOUT}]
	    }

	    my process_request
	} emsg
    }

    method router_message {} {
	puts "router message"
	catch {
	    $pipe readable {}
	    $router readable {}
	    if {[info exists $afterid]} {
		catch {after cancel $afterid}
	    }

	    set reply [zmsg recv $router]
	    puts "Reply"
	    puts [join [zmsg dump $reply] \n]

	    #  Frame 0 is server that replied
	    set endpoint [zmsg pop reply]
	    if {![info exists servers($endpoint)]} {
		error "server for endpoint '$endpoint' not found"
	    }
	    if {[$servers($endpoint) activate] == 0} {
		lappend actives $servers($endpoint)
	    }

	    #  Frame 1 may be sequence number for reply
	    set nsequence [zmsg pop reply]
	    puts "$sequence == $nsequence"
	    if {$sequence == $nsequence} {
		set reply [zmsg push $reply "OK"]
		zmsg send $pipe $reply
		set request {}
	    }

	    my process_request
	} emsg
    }

    method process_request {} {
	puts "process request"
	catch {

	    if {[llength $request]} {
		puts [join [zmsg dump $request] \n]
		if {[clock milliseconds] >= $expires} {
		    #  Request expired, kill it
		    $pipe send "FAILED"
		    set request {}
		} else {
		    #  Find server to talk to, remove any expired ones
		    while {[llength $actives]} {
			set server [lindex $actives 0]
			if {[clock milliseconds] >= [$server expires]} {
			    $server no_longer_alive
			    set actives [lassign $actives server]
			} else {
			    set nrequest $request
			    set nrequest [zmsg push $nrequest [$server endpoint]]
			    zmsg send $router $nrequest
			    break
			}
		    }
		}
	    }

	    foreach server [array names servers] {
		$servers($server) ping $router
	    }
	    set t [my get_timeout]
	    $pipe readable [list [self] control_message]
	    $router readable [list [self] router_message]
	    puts "after $t"
	    set afterid [after $t [list [self] poll]]
	} emsg
    }

    method poll {} {
	puts "poll"

	$pipe readable {}
	$router readable {}
	if {[info exists $afterid]} {
	    catch {after cancel $afterid}
	}

	my process_request
    }

    method get_timeout {} {
        #  Calculate tickless timer, up to 1 hour
	set tickless [expr {[clock milliseconds] + 1000 * 3600}]
	if {[llength $request] && $tickless > $expires} {
	    set tickless $expires
	}
	foreach {endpoint server} [array get servers] {
	    set ping_at [$server ping_at]
	    if {$tickless > $ping_at} {
		set tickless $ping_at
	    }
	}
	set timeout [expr {$tickless - [clock milliseconds]}]
	if {$timeout < 0} {
	    set timeout 0
	}
	return $timeout
    }

    #  Asynchronous agent manages server pool and handles request/reply
    #  dialog when the application asks for it.
    method process {} {
	$pipe readable [list [self] control_message]
	$router readable [list [self] router_message]
	set t [my get_timeout]
	set afterid [after $t [list [self] poll]]
    }
}
