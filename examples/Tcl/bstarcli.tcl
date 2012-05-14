#
#  Binary Star client
#

package require zmq

set REQUEST_TIMEOUT     1000    ;#  msecs
set SETTLE_DELAY        2000    ;#  Before failing over, msecs

zmq context context
set server [list "tcp://localhost:5001" "tcp://localhost:5002"]
set server_nbr 0

puts "I: connecting to server at [lindex $server $server_nbr]..."
zmq socket client context REQ
client connect [lindex $server $server_nbr]

set sequence 0
while {1} {
    #  We send a request, then we work to get a reply
    set request [incr sequence]
    client send $request

    set expect_reply 1
    while {$expect_reply} {
	#  Poll socket for a reply, with timeout
	set rpoll_set [zmq poll {{client {POLLIN}}} $REQUEST_TIMEOUT]

	#  If we got a reply, process it
	if {[llength $rpoll_set] && "POLLIN" in [lindex $rpoll_set 0 1]} {
	    set reply [client recv]
	    if {$reply eq $request} {
		puts "I: server replied OK ($reply)"
		set expect_reply 0
		after 1000 ;#  One request per second
	    } else {
		puts "E: malformed reply from server: $reply"
	    }
	} else {
	    puts "W: no response from server, failing over"
	    #  Old socket is confused; close it and open a new one
	    client close
	    set server_nbr [expr {($server_nbr + 1) % 2}]
	    after $SETTLE_DELAY
	    puts "I: connecting to server at [lindex $server $server_nbr]..."
	    zmq socket client context REQ
	    client connect [lindex $server $server_nbr]

	    #  Send request again, on new socket
	    client send $request
	}
    }
}

client close
context term
