#
# Broker peering simulation (part 3)
# Prototypes the full flow of status and tasks
#

package require zmq

if {[llength $argv] < 2} {
    puts "Usage: peering2.tcl <main|client|worker> <self> <peer ...>"
    exit 1
}

set NBR_CLIENTS 10
set NBR_WORKERS 3
set LRU_READY   "READY" ; # Signals worker is ready

set peers [lassign $argv what self]
set tclsh [info nameofexecutable]
expr {srand([pid])}

switch -exact -- $what {
    client {
	# Request-reply client using REQ socket
	# To simulate load, clients issue a burst of requests and then
	# sleep for a random period.
	#
	zmq context context
	zmq socket client context REQ
	client connect "ipc://$self-localfe.ipc"
	zmq socket monitor context PUSH
	monitor connect "ipc://$self-monitor.ipc"

	proc process_client {} {
	    global task_id done self
	    client readable {}
	    set reply [client recv]
	    if {$task_id ne [lindex $reply 0]} {
		monitor send "E [clock seconds]: CLIENT EXIT - reply '$reply' not equal to task-id '$task_id'"
		exit 1
	    }
	    monitor send "OK [clock seconds]: CLIENT REPLY - $reply"
	    set_done 1
	}

	proc set_done {v} {
	    global done
	    if {$done < 0} {
		set done $v
	    }
	}

	while {1} {
	    after [expr {int(rand()*5)*1000}]
	    set burst [expr {int(rand()*15)}]
	    while {$burst} {
		set task_id [format "%04X" [expr {int(rand()*0x10000)}]]

		#  Send request with random hex ID
		client send $task_id

		#  Wait max ten seconds for a reply, then complain
		set done -1
		client readable process_client
		set aid [after 10000 [list set_done 0]]

		vwait done
		catch {after cancel $aid}

		if {$done == 0} {
		    monitor send "E [clock seconds]: CLIENT EXIT - lost task '$task_id'"
		    exit 1
		}

		incr burst -1
	    }
	}

	client close
	control close
	context term
    }
    worker {
	#  Worker using REQ socket to do LRU routing
	#
	zmq context context
	zmq socket worker context REQ
	worker connect "ipc://$self-localbe.ipc"

	# Tell broker we're ready for work
	worker send $LRU_READY

	# Process messages as they arrive
	while {1} {
	    #  Workers are busy for 0/1 seconds
	    set msg [zmsg recv worker]
	    set payload [list [lindex $msg end] $self]
	    lset msg end $payload
	    after [expr {int(rand()*2)*1000}]
	    zmsg send worker $msg
	}

	worker close
	context term
    }
    main {
	puts "I: preparing broker at $self..."

	# Prepare our context and sockets
	zmq context context

	# Bind cloud frontend to endpoint
	zmq socket cloudfe context ROUTER
	cloudfe setsockopt IDENTITY $self
	cloudfe bind "ipc://$self-cloud.ipc"

	# Bind state backend / publisher to endpoint
	zmq socket statebe context PUB
	statebe bind "ipc://$self-state.ipc"

	# Connect cloud backend to all peers
	zmq socket cloudbe context ROUTER
	cloudbe setsockopt IDENTITY $self
	foreach peer $peers {
	    puts "I: connecting to cloud frontend at '$peer'"
	    cloudbe connect "ipc://$peer-cloud.ipc"
	}

	# Connect statefe to all peers
	zmq socket statefe context SUB
	statefe setsockopt SUBSCRIBE ""
	foreach peer $peers {
	    puts "I: connecting to state backend at '$peer'"
	    statefe connect "ipc://$peer-state.ipc"
	}

	# Prepare local frontend and backend
	zmq socket localfe context ROUTER
	localfe bind "ipc://$self-localfe.ipc"

	zmq socket localbe context ROUTER
	localbe bind "ipc://$self-localbe.ipc"

	# Prepare monitor socket
	zmq socket monitor context PULL
	monitor bind "ipc://$self-monitor.ipc"

	# Start local workers
	for {set worker_nbr 0} {$worker_nbr < $NBR_WORKERS} {incr worker_nbr} {
	    puts "Starting worker $worker_nbr, output redirected to worker-$self-$worker_nbr.log"
	    exec $tclsh peering3.tcl worker $self {*}$peers > worker-$self-$worker_nbr.log 2>@1 &
	}

	# Start local clients
	for {set client_nbr 0} {$client_nbr < $NBR_CLIENTS} {incr client_nbr} {
	    puts "Starting client $client_nbr, output redirected to client-$self-$client_nbr.log"
	    exec $tclsh peering3.tcl client $self {*}$peers > client-$self-$client_nbr.log 2>@1 &
	}

	# Interesting part
	# -------------------------------------------------------------
	# Publish-subscribe flow
	# - Poll statefe and process capacity updates
	# - Each time capacity changes, broadcast new value
	# Request-reply flow
	# - Poll primary and process local/cloud replies
	# - While worker available, route localfe to local or cloud

	# Queue of available workers
	set local_capacity 0
	set cloud_capacity 0
	set old_cloud_capacity -1
	set workers {}

	proc route_to_cloud_or_local {msg} {
	    global peers
	    # Route reply to cloud if it's addressed to a broker
	    foreach peer $peers {
		if {$peer eq [lindex $msg 0]} {
		    zmsg send cloudfe $msg
		    return
		}
	    }
	    # Route reply to client if we still need to
            zmsg send localfe $msg
	}

	proc handle_localbe {} {
	    global workers
	    # Handle reply from local worker
	    set msg [zmsg recv localbe]
	    set address [zmsg unwrap msg]
	    lappend workers $address
	    # If it's READY, don't route the message any further
	    if {[lindex $msg 0] ne "READY"} {
		route_to_cloud_or_local $msg
	    }
	}

	proc handle_cloudbe {} {
	    # Or handle reply from peer broker
	    set msg [zmsg recv cloudbe]
	    # We don't use peer broker address for anything
	    zmsg unwrap msg
	    route_to_cloud_or_local $msg
	}

	proc handle_statefe {} {
	    global cloud_capacity
	    # Handle capacity updates
	    set peer [statefe recv]
	    set cloud_capacity [statefe recv]
	}

	proc handle_monitor {} {
	    # Handle monitor message
	    puts [monitor recv]
	}

	# Now route as many clients requests as we can handle
	# - If we have local capacity we poll both localfe and cloudfe
	# - If we have cloud capacity only, we poll just localfe
	# - Route any request locally if we can, else to cloud
	#
	proc handle_client {s} {
	    global peers workers workers cloud_capacity self
	    set msg [zmsg recv $s]
	    if {[llength $workers]} {
		set workers [lassign $workers frame]
		set msg [zmsg wrap $msg $frame]
		zmsg send localbe $msg
	    } else {
		set peer [lindex $peers [expr {int(rand()*[llength $peers])}]]
		set msg [zmsg push $msg $peer]
		zmsg send cloudbe $msg
	    }
	}

	proc handle_clients {} {
	    if {[catch {
	    global workers cloud_capacity
	    if {[llength $workers] && ("POLLIN" in [cloudfe getsockopt EVENTS])} {
		handle_client cloudfe
	    }
	    if {([llength $workers] || $cloud_capacity) && ("POLLIN" in [localfe getsockopt EVENTS])} {
		handle_client localfe
	    }
	    } msg]} {
		puts $msg
	    }
	}

	proc publish_capacity {} {
	    global self workers old_cloud_capacity
	    if {[llength $workers] != $old_cloud_capacity} {
		puts "OK [clock seconds] : PUBLISH CAPACITY [llength $workers]"
		# We stick our own address onto the envelope
		statebe sendmore $self
		# Broadcast new capacity
		statebe send [llength $workers]
		set old_cloud_capacity [llength $workers]
	    }
	    # Repeat
	    after 1000 publish_capacity
	}

	localbe readable handle_localbe
	cloudbe readable handle_cloudbe
	statefe readable handle_statefe
	monitor readable handle_monitor

	localfe readable handle_clients
	cloudfe readable handle_clients

	publish_capacity

	vwait forever

	# When we're done, clean up properly
	localbe close
	localfe close
	cloudbe close
	cloudfe close
	monitor close
	statefe close
	context term
    }
}
