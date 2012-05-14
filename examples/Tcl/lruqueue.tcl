#
# Least-recently used (LRU) queue device
#
package require zmq

if {[llength $argv] == 0} {
    set argv [list driver 0 3 5]
} elseif {[llength $argv] != 4} {
    puts "Usage: lruqueue.tcl <driver|client|worker|main_sync|main_async> <asynchronous> <number_of_clients> <number_of_workers>"
    exit 1
}

set tclsh [info nameofexecutable]
lassign $argv what asynchronous NBR_CLIENTS NBR_WORKERS
expr {srand([pid])}

switch -exact -- $what {
    client {
	# Basic request-reply client using REQ socket
	# Since send and recv can't handle 0MQ binary identities we
	# set a printable text identity to allow routing.

	package require zmq

	zmq context context

	zmq socket client context REQ
	set id [format "%04X-%04X" [expr {int(rand()*0x10000)}] [expr {int(rand()*0x10000)}]]
	client setsockopt IDENTITY $id
	client connect "ipc://frontend.ipc"

	# Send request, get reply
	client send "HELLO"
	set reply [client recv]
	puts "Client $id: $reply"

	client close
	context term
    }
    worker {
	# Worker using REQ socket to do LRU routing
	# Since send and recv can't handle 0MQ binary identities we
	# set a printable text identity to allow routing.

	zmq context context

	zmq socket worker context REQ
	set id [format "%04X-%04X" [expr {int(rand()*0x10000)}] [expr {int(rand()*0x10000)}]]
	worker setsockopt IDENTITY $id
	worker connect "ipc://backend.ipc"

	# Tell broker we're ready for work
	worker send "READY"

	while {1} {
	    # Read and save all frames until we get an empty frame
	    # In this example there is only 1 but it could be more
	    set address [worker recv]
	    set empty [worker recv]

	    # Get request, send reply
	    set request [worker recv]
	    puts "Worker $id: $request"

	    worker sendmore $address
	    worker sendmore ""
	    worker send "OK"
	}

	worker close
	context term
    }
    main_sync {
	zmq context context

	zmq socket frontend context ROUTER
	zmq socket backend context ROUTER
	frontend bind "ipc://frontend.ipc"
	backend bind "ipc://backend.ipc"

	# Logic of LRU loop
	# - Poll backend always, frontend only if 1+ worker ready
	# - If worker replies, queue worker as ready and forward reply
	#   to client if necessary
	# - If client requests, pop next worker and send request to it

	# Queue of available workers
	set client_nbr $NBR_CLIENTS
	set worker_queue {}

	set done 0
	while {!$done} {
	    if {[llength $worker_queue]} {
		set poll_set [list [list backend [list POLLIN]] [list frontend [list POLLIN]]]
	    } else {
		set poll_set [list [list backend [list POLLIN]]]
	    }
	    set rpoll_set [zmq poll $poll_set -1]
	    foreach rpoll $rpoll_set {
		switch [lindex $rpoll 0] {
		    backend {
			# Queue worker address for LRU routing
			set worker_addr [backend recv]
			if {!([llength $worker_queue] < $NBR_WORKERS)} {
			    error "available_workers < NBR_WORKERS"
			}
			lappend worker_queue $worker_addr

			# Second frame is empty
			set empty [backend recv]

			# Third frame is READY or else a client reply address
			set client_addr [backend recv]

			# If client reply, send rest back to frontend
			if {$client_addr ne "READY"} {
			    set empty [backend recv]
			    set reply [backend recv]

			    frontend sendmore $client_addr
			    frontend sendmore ""
			    frontend send $reply
			    incr client_nbr -1
			    if {$client_nbr == 0} {
				set done 1
				break
			    }
			}
		    }
		    frontend {
			# Now get next client request, route to LRU worker
			# Client request is [address][empty][request]
			set client_addr [frontend recv]
			set empty [frontend recv]
			set request [frontend recv]

			backend sendmore [lindex $worker_queue 0]
			backend sendmore ""
			backend sendmore $client_addr
			backend sendmore ""
			backend send $request

			# Dequeue and drop the next worker address
			set worker_queue [lrange $worker_queue 1 end]
		    }
		}
	    }
	}

	frontend close
	backend close
	context term
    }
    main_async {
	zmq context context

	zmq socket frontend context ROUTER
	zmq socket backend context ROUTER
	frontend bind "ipc://frontend.ipc"
	backend bind "ipc://backend.ipc"

	# Logic of LRU loop
	# - Poll backend always, frontend only if 1+ worker ready
	# - If worker replies, queue worker as ready and forward reply
	#   to client if necessary
	# - If client requests, pop next worker and send request to it

	# Queue of available workers
	set client_nbr $NBR_CLIENTS
	set worker_queue {}

	set done 0

	proc process_backend {fe be} {
	    global done worker_queue client_nbr NBR_WORKERS
	    # Queue worker address for LRU routing
	    set worker_addr [$be recv]
	    if {!([llength $worker_queue] < $NBR_WORKERS)} {
		error "available_workers < NBR_WORKERS"
	    }
	    lappend worker_queue $worker_addr

	    # Second frame is empty
	    set empty [$be recv]

	    # Third frame is READY or else a client reply address
	    set client_addr [$be recv]

	    # If client reply, send rest back to frontend
	    if {$client_addr ne "READY"} {
		set empty [$be recv]
		set reply [$be recv]

		$fe sendmore $client_addr
		$fe sendmore ""
		$fe send $reply
		incr client_nbr -1
		if {$client_nbr == 0} {
		    set ::done 1
		    break
		}
	    }
	}

	proc process_frontend {fe be} {
	    global done worker_queue client_nbr
	    if {[llength $worker_queue]} {
		# Now get next client request, route to LRU worker
		# Client request is [address][empty][request]
		set client_addr [$fe recv]
		set empty [$fe recv]
		set request [$fe recv]

		$be sendmore [lindex $worker_queue 0]
		$be sendmore ""
		$be sendmore $client_addr
		$be sendmore ""
		$be send $request

		# Dequeue and drop the next worker address
		set worker_queue [lrange $worker_queue 1 end]
	    }
	}

	frontend readable [list process_frontend ::frontend ::backend]
	backend readable [list process_backend ::frontend ::backend]

	vwait done

	frontend close
	backend close
	context term
    }
    driver {
	puts "Start main, output redirect to main.log"
	exec $tclsh lruqueue.tcl [expr {$asynchronous?"main_async":"main_sync"}] $asynchronous $NBR_CLIENTS $NBR_WORKERS > main.log 2>@1 &

	after 1000

	for {set i 0} {$i < $NBR_WORKERS} {incr i} {
	    puts "Start worker $i, output redirect to worker$i.log"
	    exec $tclsh lruqueue.tcl worker $asynchronous $NBR_CLIENTS $NBR_WORKERS > worker$i.log 2>@1 &
	}

	after 1000

	for {set i 0} {$i < $NBR_CLIENTS} {incr i} {
	    puts "Start client $i, output redirect to client$i.log"
	    exec $tclsh lruqueue.tcl client $asynchronous $NBR_CLIENTS $NBR_WORKERS > client$i.log 2>@1 &
	}
    }
}
