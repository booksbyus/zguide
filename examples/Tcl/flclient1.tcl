#
#  Freelance client - Model 1
#  Uses REQ socket to query one or more services
#

package require zmq

set REQUEST_TIMEOUT     1000
set MAX_RETRIES         3       ;#  Before we abandon

if {[llength $argv] == 0} {
    puts "Usage: flclient1.tcl <endpoint> ..."
    exit 1
}

proc s_try_request {ctx endpoint request} {
    puts "I: trying echo service at $endpoint..."
    zmq socket client $ctx REQ
    client connect $endpoint

    #  Send request, wait safely for reply
    zmsg send client $request
    set reply {}
    set rpoll_set [zmq poll {{client {POLLIN}}} $::REQUEST_TIMEOUT]
    if {[llength $rpoll_set] && "POLLIN" in [lindex $rpoll_set 0 1]} {
	set reply [zmsg recv client]
    }

    #  Close socket in any case, we're done with it now
    client setsockopt LINGER 0
    client close
    return $reply
}

zmq context context

set request {}
set request [zmsg add $request "Hello World"]
set reply {}

if {[llength $argv] == 1} {
    #  For one endpoint, we retry N times
    set endpoint [lindex $argv 0]
    for {set retries 0} {$retries < $MAX_RETRIES} {incr retries} {
	set reply [s_try_request context $endpoint $request]
	if {[llength $reply]} {
	    break ;# Successful
	}
	puts "W: no response from $endpoint, retrying..."
    }
} else {
    #  For multiple endpoints, try each at most once
    foreach endpoint $argv {
	set reply [s_try_request context $endpoint $request]
	if {[llength $reply]} {
	    break ;# Successful
	}
	puts "W: no response from $endpoint"
    }
}

if {[llength $reply]} {
    puts "Service is running OK"
}

context term
