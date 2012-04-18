#
# Asynchronous client-to-server (DEALER to ROUTER)
#

if {[llength $argv] == 0} {
    set argv [list driver 3 5]
} elseif {[llength $argv] != 3} {
    puts "Usage: asyncsrv.tcl ?<driver|client|server|worker> <number_of_clients> <number_of_workers>?"
    exit 1
}

set tclsh [info nameofexecutable]
lassign $argv what NBR_CLIENTS NBR_WORKERS
expr {srand([pid])}

switch -exact -- $what {
    client {
	# This is our client task
	# It connects to the server, and then sends a request once per second
	# It collects responses as they arrive, and it prints them out. We will
	# run several client tasks in parallel, each with a different random ID.

	package require zmq

	zmq context context
	zmq socket client context DEALER

	# Set random identity to make tracing easier
	set identity [format "%04X-%04X" [expr {int(rand()*0x10000)}] [expr {int(rand()*0x10000)}]]
	client setsockopt IDENTITY $identity
	client connect "tcp://localhost:5570"

	proc receive {} {
	    global identity
	    puts "Client $identity received [client recv]"
	}

	proc request {} {
	    global request_nbr identity
	    incr request_nbr
	    puts "Client $identity sent request \#$request_nbr"
	    client send "request \#$request_nbr"
	    after 1000 "request"
	}

	# Process responses
	client readable receive

	# Send a request every second
	set request_nbr 0
	after 1000 request

	vwait forever

	client close
	context term
    }
    worker {
	# This is our worker task
	# Accept a request and reply with the same text a random number of
	# times, with random delays between replies.

	package require zmq

	zmq context context
	zmq socket worker context DEALER
	worker connect "ipc://backend"

	while {1} {
	    # The DEALER socket gives us the address envelope and message
	    set address [worker recv]
	    set content [worker recv]

	    puts "worker received $content from $address"

	    # Send 0..4 replies back
	    set replies [expr {int(rand()*5)}]
	    for {set reply 0} {$reply < $replies} {incr reply} {
		# Sleep for some fraction of a second
		after [expr {int(rand()*1000) + 1}]
		puts "worker send $content to $address"
		worker sendmore $address
		worker send $content
	    }
	}
    }
    server {
	# This is our server task It uses the multithreaded server model to deal
	# requests out to a pool of workers and route replies back to clients. One
	# worker can handle one request at a time but one client can talk to multiple
	# workers at once.

	package require zmq

	zmq context context

	# Frontend socket talks to clients over TCP
	zmq socket frontend context ROUTER
	frontend bind "tcp://*:5570"

	# Backend socket talks to workers over inproc
	zmq socket backend context DEALER
	backend bind "ipc://backend"

	#  Launch pool of worker threads, precise number is not critical
	for {set thread_nbr 0} {$thread_nbr < $NBR_WORKERS} {incr thread_nbr} {
	    exec $tclsh asyncsrv.tcl worker $NBR_CLIENTS $NBR_WORKERS > worker$thread_nbr.log 2>@1 &
	}

	#  Connect backend to frontend via a queue device
	#  We could do this:
	#      zmq_device (ZMQ_QUEUE, frontend, backend);
	#  But doing it ourselves means we can debug this more easily

	proc do_frontend {} {
	    set address [frontend recv]
	    set data [frontend recv]

	    backend sendmore $address
	    backend send $data
	}

	proc do_backend {} {
	    set address [backend recv]
	    set data [backend recv]

	    frontend sendmore $address
	    frontend send $data
	}

	backend readable do_backend
	frontend readable do_frontend
	vwait forever

	frontend close
	backend close
	context term
    }
    driver {
	puts "Start server, output redirected to server.log"
	exec $tclsh asyncsrv.tcl server $NBR_CLIENTS $NBR_WORKERS > server.log 2>@1 &

	after 1000

	for {set i 0} {$i < $NBR_CLIENTS} {incr i} {
	    puts "Start client $i, output redirect to client$i.log"
	    exec $tclsh asyncsrv.tcl client $NBR_CLIENTS $NBR_WORKERS > client$i.log 2>@1 &
	}
    }
}
