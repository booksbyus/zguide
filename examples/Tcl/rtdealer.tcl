#  Custom routing Router to Dealer
package require zmq

if {[llength $argv] == 0} {
    set argv [list main {}]
} elseif {[llength $argv] != 2} {
    puts "Usage: rtdelaer.tcl <worker|main> <identity>"
    exit 1
}

set tclsh [info nameofexecutable]
lassign $argv what identity
expr {srand([pid])}

switch -exact -- $what {
    worker {
	zmq context context

	zmq socket worker context DEALER
	worker setsockopt IDENTITY $identity
	worker connect "ipc://routing.ipc"

	set total 0
	while {1}  {
	    # We receive one part, with the workload
	    set request [worker recv]
	    if {$request eq "END"} {
		puts "$identity received: $total"
		break;
	    }
	    incr total
	}

	worker close
	context term
    }
    main {
	zmq context context

	zmq socket client context ROUTER
	client bind "ipc://routing.ipc"

	foreach c {A B} {
	    puts "Start worker $c, output redirected to worker$c.log"
	    exec $tclsh rtdealer.tcl worker $c > worker$c.log 2>@1 &
	}

	# Wait for threads to connect, since otherwise the messages
	# we send won't be routable.
	after 1000

	# Send 10 tasks scattered to A twice as often as B
	for {set task_nbr 0} {$task_nbr < 10} {incr task_nbr} {
	    # Send two message parts, first the address…
	    set id [expr {int(rand() * 3) > 0?"A":"B"}]
	    client sendmore $id

	    # And then the workload
	    client send "This is the workload"
	}

	client sendmore "A"
	client send "END"

	client sendmore "B"
	client send "END"

	client close
	context term
    }
}
