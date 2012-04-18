#
#  Round-trip demonstrator
#

package require zmq

if {[llength $argv] == 0} {
    set argv [list driver]
} elseif {[llength $argv] != 1} {
    puts "Usage: tripping.tcl <driver|worker|client|broker>"
    exit 1
}

set tclsh [info nameofexecutable]
expr {srand([pid])}

lassign $argv what

switch -exact -- $what {
    client {
	zmq context context
	zmq socket client context DEALER
	client setsockopt IDENTITY "C"
	client connect "tcp://localhost:5555"

	puts "Setting up test..."
	after 1000

	puts "Synchronous round-trip test..."
	set start [clock milliseconds]
	for {set requests 0} {$requests < 10000} {incr requests} {
	    client send "hello"
	    client recv
	}
	puts "[expr {1000.0*10000/([clock milliseconds] - $start)}] calls/second"

	puts "Asynchronous round-trip test..."
	set start [clock milliseconds]
	for {set requests 0} {$requests < 10000} {incr requests} {
	    client send "hello"
	}
	for {set requests 0} {$requests < 10000} {incr requests} {
	    client recv
	}
	puts "[expr {1000.0*10000/([clock milliseconds] - $start)}] calls/second"

	puts "Callback round-trip test..."

	proc recv_client {} {
	    client recv
	    incr ::cnt
	    if {$::cnt == 10000} {
		set ::done 1
	    }
	}
		      

	set start [clock milliseconds]
	client readable recv_client
	for {set requests 0} {$requests < 10000} {incr requests} {
	    client send "hello"
	}
	vwait done
	puts "[expr {1000.0*10000/([clock milliseconds] - $start)}] calls/second"
    }
    worker {
	zmq context context
	zmq socket worker context DEALER
	worker setsockopt IDENTITY "W"
	worker connect "tcp://localhost:5556"

	while {1} {
	    zmsg send worker [zmsg recv worker]
	}

	worker close
	context term
    }
    broker {
	zmq context context
	zmq socket frontend context ROUTER
	zmq socket backend context ROUTER
	frontend bind "tcp://*:5555"
	backend bind "tcp://*:5556"

	while {1} {
	    set poll_set [list [list backend [list POLLIN]] [list frontend [list POLLIN]]]
	    set rpoll_set [zmq poll $poll_set -1]
	    foreach rpoll $rpoll_set {
		switch [lindex $rpoll 0] {
		    backend {
			set msg [zmsg recv backend]
			set address [zmsg pop msg]
			set msg [zmsg push $msg "C"]
			zmsg send frontend $msg
		    }
		    frontend {
			set msg [zmsg recv frontend]
			set address [zmsg pop msg]
			set msg [zmsg push $msg "W"]
			zmsg send backend $msg
		    }
		}
	    }
	}

	frontend close
	backend close
	context term
    }
    driver {
	exec $tclsh tripping.tcl client > client.log 2>@1 &
	exec $tclsh tripping.tcl worker > worker.log 2>@1 &
	exec $tclsh tripping.tcl broker > broker.log 2>@1 &
    }
}
