#
#  Titanic service
#
#  Implements server side of http://rfc.zeromq.org/spec:9

lappend auto_path .
package require MDClient 1.0
package require MDWorker 1.0
package require uuid

if {[llength $argv] == 0} {
    set argv [list driver]
} elseif {[llength $argv] != 1} {
    puts "Usage: titanic.tcl <driver|request|reply|close>"
    exit 1
}

set tclsh [info nameofexecutable]
expr {srand([pid])}
set verbose 0

lassign $argv what

set TITANIC_DIR ".titanic"

#  Return a new UUID as a printable character string
proc s_generate_uuid {} {
    return [uuid::uuid generate]
}

#  Returns freshly allocated request filename for given UUID
proc s_request_filename {uuid} {
    return [file join $::TITANIC_DIR $uuid.req]
}

#  Returns freshly allocated reply filename for given UUID
proc s_reply_filename {uuid} {
    return [file join $::TITANIC_DIR $uuid.rep]
}

#  Titanic request service
proc titanic_request {} {

    zmq context context
    set pipe [zmq socket pipe context PAIR]
    pipe connect "ipc://titanicpipe.ipc"

    set worker [MDWorker new "tcp://localhost:5555" "titanic.request" $::verbose]
    set reply {}

    while {1} {
        #  Send reply if it's not null
        #  And then get next request from broker
	set request [$worker recv $reply]
	if {[llength $request] == 0} {
	    break ;# Interrupted, exit
	}

        #  Ensure message directory exists
	file mkdir $::TITANIC_DIR

	#  Generate UUID and save message to disk
	set uuid [s_generate_uuid]
	set filename [s_request_filename $uuid]
	set file [open $filename "w"]
	puts -nonewline $file [join $request \n]
	close $file

	#  Send UUID through to message queue
	set reply [list]
	set reply [zmsg add $reply $uuid]
	zmsg send $pipe $reply

        #  Now send UUID back to client
        #  Done by the mdwrk_recv() at the top of the loop
	set reply [list]
	puts "I: titanic.request to $uuid / $reply"
	set reply [zmsg add $reply "200"]
	puts "I: titanic.request to $uuid / $reply"
	set reply [zmsg add $reply $uuid]
	puts "I: titanic.request to $uuid / $reply"
	puts [join [zmsg dump $reply] \n]
    }
    $worker destroy
}

#  Titanic reply service
proc titanic_reply {} {
    set worker [MDWorker new "tcp://localhost:5555" "titanic.reply" $::verbose]
    set reply {}

    while {1} {
	set request [$worker recv $reply]
	if {[llength $request] == 0} {
	    break
	}
	set uuid [zmsg pop request]
	set req_filename [s_request_filename $uuid]
	set rep_filename [s_reply_filename $uuid]
	if {[file exists $rep_filename]} {
	    set file [open $rep_filename r]
	    set reply [split [read $file] \n]
	    set reply [zmsg push $reply "200"]
	    puts "I: titanic.reply to $uuid"
	    puts [join [zmsg dump $reply] \n]
	    close $file
	} else {
	    if {[file exists $req_filename]} {
		set reply "300"
	    } else {
		set reply "400"
	    }
	}
    }
    $worker destroy
    return 0
}

#  Titanic close service
proc titanic_close {} {
    set worker [MDWorker new "tcp://localhost:5555" "titanic.close" $::verbose]
    set reply ""

    while {1} {
	set request [$worker recv $reply]
	if {[llength $request] == 0} {
	    break
	}
	set uuid [zmsg pop request]
	set req_filename [s_request_filename $uuid]
	set rep_filename [s_reply_filename $uuid]
	file delete -force $req_filename
	file delete -force $rep_filename

	set reply "200"
    }
    $worker destroy
    return 0
}

#  Attempt to process a single request, return 1 if successful
proc s_service_success {uuid} {
    #  Load request message, service will be first frame
    set filename [s_request_filename $uuid]
    #  If the client already closed request, treat as successful
    if {![file exists $filename]} {
	return 1
    }
    set file [open $filename "r"]

    set request [split [read $file] \n]
    set service [zmsg pop request]

    #  Create MDP client session with short timeout
    set client [MDClient new "tcp://localhost:5555" $::verbose]
    $client set_timeout 1000
    $client set_retries 1

    #  Use MMI protocol to check if service is available
    set mmi_request {}
    set mmi_request [zmsg add $mmi_request $service]
    set mmi_reply [$client send "mmi.service" $mmi_request]

    if {[lindex $mmi_reply 0] eq "200"} {
	set reply [$client send $service $request]
	if {[llength $reply]} {
	    set filename [s_reply_filename $uuid]
	    set file [open $filename "w"]
	    puts -nonewline $file [join $reply \n]
	    close $file
	    return 1
	}
    }

    $client destroy
    return 0
}

switch -exact -- $what {
    request { titanic_request }
    reply { titanic_reply }
    close { titanic_close }
    driver {
	exec $tclsh titanic.tcl request > request.log 2>@1 &
	exec $tclsh titanic.tcl reply > reply.log 2>@1 &
	exec $tclsh titanic.tcl close > close.log 2>@1 &

	after 1000 ;# Wait for other parts to start

	zmq context context
	zmq socket request_pipe context PAIR
	request_pipe bind "ipc://titanicpipe.ipc"
	set queuefnm [file join $::TITANIC_DIR queue]

	#  Main dispatcher loop
	while {1} {
	    #  We'll dispatch once per second, if there's no activity
	    set poll_set [list [list request_pipe [list POLLIN]]]
	    set rpoll_set [zmq poll $poll_set 1000]
	    if {[llength $rpoll_set] && "POLLIN" in [lindex $rpoll_set 0 1]} {
		#  Ensure message directory exists
		file mkdir $::TITANIC_DIR

		#  Append UUID to queue, prefixed with '-' for pending
		set msg [zmsg recv request_pipe]
		if {[llength $msg] == 0} {
		    break
		}
		set file [open $queuefnm "a"]
		set uuid [zmsg pop msg]
		puts $file "-$uuid"
		close $file
	    }
	    #  Brute-force dispatcher
	    if {[file exists $queuefnm]} {
		set file [open $queuefnm "r"]
		set queue_list [split [read $file] \n]
		close $file
		for {set i 0} {$i < [llength $queue_list]} {incr i} {
		    set entry [lindex $queue_list $i]
		    if {[string match "-*" $entry]} {
			set entry [string range $entry 1 end]
			puts "I: processing request $entry"
			if {[s_service_success $entry]} {
			    lset queue_list $i "+$entry"
			}
		    }
		}
		set file [open $queuefnm "w"]
		puts -nonewline $file [join $queue_list \n]
		close $file
	    }
	}

	return 0
    }
}
