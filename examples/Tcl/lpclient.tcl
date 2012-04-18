#
#  Lazy Pirate client
#  Use zmq_poll to do a safe request-reply
#  To run, start lpserver and then randomly kill/restart it
#

package require zmq

set REQUEST_TIMEOUT     2500    ;#  msecs, (> 1000!)
set REQUEST_RETRIES     3       ;#  Before we abandon
set SERVER_ENDPOINT     "tcp://localhost:5555"

zmq context context
puts "I: connecting to server..."
zmq socket client context REQ
client connect $SERVER_ENDPOINT

set sequence 0
set retries_left $REQUEST_RETRIES

while {$retries_left} {
    #  We send a request, then we work to get a reply
    client send [incr sequence]

    set expect_reply 1
    while {$expect_reply} {
	#  Poll socket for a reply, with timeout
	set rpoll_set [zmq poll {{client {POLLIN}}} $REQUEST_TIMEOUT]

	#  If we got a reply, process it
	if {[llength $rpoll_set] && [lindex $rpoll_set 0 0] eq "client"} {
	    #  We got a reply from the server, must match sequence
	    set reply [client recv]
	    if {$reply eq $sequence} {
		puts "I: server replied OK ($reply)"
		set retries_left $REQUEST_RETRIES
		set expect_reply 0
	    } else {
		puts "E: malformed reply from server: $reply"
	    }
	} elseif {[incr retries_left -1] <= 0} {
	    puts "E: server seems to be offline, abandoning"
	    set retries_left 0
	    break
	} else {
	    puts "W: no response from server, retrying..."
	    #  Old socket is confused; close it and open a new one
	    client close
	    puts "I: connecting to server..."
	    zmq socket client context REQ
	    client connect $SERVER_ENDPOINT
	    #  Send request again, on new socket
	    client send $sequence
	}
    }
}

client close
context term
