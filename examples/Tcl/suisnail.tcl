#
#  Suicidal Snail
#

package require zmq

if {[llength $argv] == 0} {
    set argv [list driver]
} elseif {[llength $argv] != 1} {
    puts "Usage: suisnail.tcl <driver|sub|pub>"
    exit 1
}

lassign $argv what
set MAX_ALLOWED_DELAY   1000    ;#  msecs

set tclsh [info nameofexecutable]
expr {srand([pid])}

switch -exact -- $what {
    sub {
	#  This is our subscriber
	#  It connects to the publisher and subscribes to everything. It
	#  sleeps for a short time between messages to simulate doing too
	#  much work. If a message is more than 1 second late, it croaks.
	zmq context context
	zmq socket subpipe context PAIR
	subpipe connect "ipc://subpipe.ipc"
	#  Subscribe to everything
	zmq socket subscriber context SUB
	subscriber setsockopt SUBSCRIBE ""
	subscriber connect "tcp://localhost:5556"

	#  Get and process messages
	while {1} {
	    set string [subscriber recv]
	    puts "$string (delay = [expr {[clock milliseconds] - $string}])"

	    if {[clock milliseconds] - $string > $::MAX_ALLOWED_DELAY} {
		puts stderr "E: subscriber cannot keep up, aborting"
		break
	    }

	    after [expr {1+int(rand()*2)}]
	}

	subpipe send "gone and died"

	subscriber close
	subpipe close
	context term
    }
    pub {
	#  This is our server task
	#  It publishes a time-stamped message to its pub socket every 1ms.
	zmq context context
	zmq socket pubpipe context PAIR
	pubpipe connect "ipc://pubpipe.ipc"

	#  Prepare publisher
	zmq socket publisher context PUB
	publisher bind "tcp://*:5556"

	while {1} {
	    #  Send current clock (msecs) to subscribers
	    publisher send [clock milliseconds]

	    if {"POLLIN" in [pubpipe getsockopt EVENTS]} {
		break
	    }

	    after 1 ;# 1msec wait
	}

	publisher close
	pubpipe close
	context term
    }
    driver {
	zmq context context
	zmq socket pubpipe context PAIR
	pubpipe bind "ipc://pubpipe.ipc"
	zmq socket subpipe context PAIR
	subpipe bind "ipc://subpipe.ipc"
	puts "Start publisher, output redirected to publisher.log"
	exec $tclsh suisnail.tcl pub > publisher.log 2>@1 &
	puts "Start subscriber, output redirected to subscriber.log"
	exec $tclsh suisnail.tcl sub > subscriber.log 2>@1 &
	subpipe recv
	pubpipe send "break"
	after 100
	pubpipe close
	subpipe close
	context term
    }
}
