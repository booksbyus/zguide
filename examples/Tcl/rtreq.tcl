#
# Custom routing Router to Mama (ROUTER to REQ)
#

package require zmq

if {[llength $argv] == 0} {
    set argv [list driver 3]
} elseif {[llength $argv] != 2} {
    puts "Usage: rtmama.tcl <driver|main|worker> <number_of_workers>"
    exit 1
}

lassign $argv what NBR_WORKERS

set tclsh [info nameofexecutable]
set nbr_of_workers [lindex $argv 0]
expr {srand([pid])}

switch -exact -- $what {
    worker {
	zmq context context

	zmq socket worker context REQ

	# We use a string identity for ease here
	set id [format "%04X-%04X" [expr {int(rand()*0x10000)}] [expr {int(rand()*0x10000)}]]
	worker setsockopt IDENTITY $id
	worker connect "ipc://routing.ipc"

	set total 0
	while {1} {
	    # Tell the router we're ready for work
	    worker send "ready"

	    # Get workload from router, until finished
	    set workload [worker recv]
	    if {$workload eq "END"} {
		puts "Processed: $total tasks"
		break
	    }
	    incr total

	    # Do some random work
	    after [expr {int(rand()*1000)}]
	}

	worker close
	context term
    }
    main {
	zmq context context

	zmq socket client context ROUTER
	client bind "ipc://routing.ipc"

	for {set task_nbr 0} {$task_nbr < $NBR_WORKERS * 10} {incr task_nbr} {
	    # LRU worker is next waiting in queue
	    set address [client recv]
	    set empty [client recv]
	    set ready [client recv]
	    client sendmore $address
	    client sendmore ""
	    client send "This is the workload"
	}

	# Now ask mamas to shut down and report their results
	for {set worker_nbr 0} {$worker_nbr < $NBR_WORKERS} {incr worker_nbr} {
	    set address [client recv]
	    set empty [client recv]
	    set ready [client recv]
	    client sendmore $address
	    client sendmore ""
	    client send "END"
	}

	client close
	context term
    }
    driver {
	puts "Start main, output redirected to main.log"
	exec $tclsh rtmama.tcl main $NBR_WORKERS > main.log 2>@1 &

	after 1000

	for {set i 0} {$i < $NBR_WORKERS} {incr i} {
	    puts "Start worker $i, output redirected to worker$i.log"
	    exec $tclsh rtmama.tcl worker $NBR_WORKERS > worker$i.log 2>@1 &
	}
    }
}
