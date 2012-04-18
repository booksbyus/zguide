#
#  Freelance client - Model 2
#  Uses DEALER socket to blast one or more services
#

lappend auto_path .
package require TclOO
package require zmq
package require mdp

if {[llength $argv] == 0} {
    puts "Usage: flclient2.tcl <endpoint> ..."
    exit 1
}

#  If not a single service replies within this time, give up
set GLOBAL_TIMEOUT 2500

oo::class create FLClient {

    variable ctx socket servers sequence

    constructor {} {
	set ctx [zmq context mdcli_context_[::mdp::contextid]]
	set socket [zmq socket mdcli_socket_[::mdp::socketid] $ctx DEALER]
	set servers 0
	set sequence 0
    }

    destructor {
	$socket setsockopt LINGER 0
	$socket close
	$ctx term
    }

    method connect {endpoint} {
	$socket connect $endpoint
	incr servers
    }

    #  Send request, get reply
    method request {request} {
	#  Prefix request with sequence number and empty envelope
	set request [zmsg push $request [incr sequence]]
	set request [zmsg push $request ""]

	#  Blast the request to all connected servers
	for {set server 0} {$server < $servers} {incr server} {
	    zmsg send $socket $request
	}

	#  Wait for a matching reply to arrive from anywhere
	#  Since we can poll several times, calculate each one
	set reply {}
	set endtime [expr {[clock milliseconds] + $::GLOBAL_TIMEOUT}]
	while {[clock milliseconds] < $endtime} {
	    set rpoll_set [zmq poll [list [list $socket {POLLIN}]] [expr {($endtime - [clock milliseconds])}]]
	    if {[llength $rpoll_set] && "POLLIN" in [lindex $rpoll_set 0 1]} {
		#  Reply is [empty][sequence][OK]
		set reply [zmsg recv $socket]
		if {[llength $reply] != 3} {
		    error "expected reply with length 3"
		}
		zmsg pop reply
		set rsequence [zmsg pop reply]
		if {$rsequence == $sequence} {
		    break
		}
	    }
	}
	return $reply
    }
}

#  Create new freelance client object
set client [FLClient new]

#  Connect to each endpoint
foreach endpoint $argv {
    $client connect $endpoint
}

#  Send a bunch of name resolution 'requests', measure time
set requests 100
set start [clock microseconds]
for {set i 0} {$i < $requests} {incr i} {
    set request {}
    set request [zmsg add $request "random name"]
    set reply [$client request $request]
    if {[llength $reply] == 0} {
	puts "E: name service not available, aborting"
	break
    }
}
puts "Average round trip cost: [expr {([clock microseconds] - $start) / $requests}] usec"

$client destroy
