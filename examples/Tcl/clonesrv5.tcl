#
#  Clone server Model Five
#

lappend auto_path .
package require TclOO
package require mdp
package require KVMsg

oo::class create CloneServer {

    variable ctx kvmap sequence snapshot publisher collector afterid

    constructor {port} {
	#  Set up our clone server sockets
	set sequence 0
	set ctx [zmq context cloneserver_context_[mdp::contextid]]
	set snapshot [zmq socket clonserver_snapshot[mdp::socketid] $ctx ROUTER]
	set publisher [zmq socket cloneserver_publisher_[mdp::socketid] $ctx PUB]
	set collector [zmq socket cloneserver_collector_[mdp::socketid] $ctx PULL]
	$snapshot bind "tcp://*:$port"
	$publisher bind "tcp://*:[expr {$port+1}]"
	$collector bind "tcp://*:[expr {$port+2}]"

	#  Register our handlers with reactor
	my register
    }

    destructor {
	$snapshot close
	$publisher close
	$collector close
	$ctx term
    }

    method register {} {
	$snapshot readable [list [self] s_snapshot]
	$collector readable [list [self] s_collector]
	set afterid [after 1000 [list [self] s_flush_ttl]]
    }

    method unregister {} {
	$snapshot readable {}
	$collector readable {}
	catch {after cancel $afterid}
    }

    #  Send snapshots to clients who ask for them
    method s_snapshot {} {
	set identity [$snapshot recv]
	if {[string length $identity]} {
	    set request [$snapshot recv]
	    if {$request eq "ICANHAZ?"} {
		set subtree [$snapshot recv]
	    } else {
		puts "E: bad request, aborting"
	    }
	    if {[info exists subtree]} {
		#  Send state to client
		foreach {key value} [array get kvmap] {
		    #  Send one state snapshot key-value pair to a socket
		    #  Hash item data is our kvmsg object, ready to send
		    if {[string match $subtree* [$value key]]} {
			$snapshot sendmore $identity
			$value send $snapshot
		    }
		}

		#  Now send END message with sequence number
		puts "I: sending snapshot=$sequence"
		$snapshot sendmore $identity
		set kvmsg [KVMsg new $sequence]
		$kvmsg set_key "KTHXBAI"
		$kvmsg set_body $subtree
		$kvmsg send $snapshot
		$kvmsg destroy
	    }
	}
    }

    #  Collect updates from clients
    method s_collector {} {
	set kvmsg [KVMsg new]
	$kvmsg recv $collector
	$kvmsg set_sequence [incr sequence]
	$kvmsg send $publisher
	set ttl [$kvmsg get_prop "ttl"]
	if {$ttl} {
	    $kvmsg set_prop "ttl" [expr {[clock milliseconds] + $ttl * 1000}]
	    $kvmsg store kvmap
	    puts "I: publishing update=$sequence"
	}
    }

    #  Purge ephemeral values that have expired
    method s_flush_ttl {} {
	foreach {key value} [array names kvmap] {
	    #  If key-value pair has expired, delete it and publish the
	    #  fact to listening clients.
	    if {[clock milliseconds] >= [$value get_prop "ttl"]} {
		$value set_sequence [incr sequence]
		$value set_body ""
		$value send $publisher
		$value stor kvmap
		puts "I: publishing delete=$sequence"
	    }
	}
    }
}

set server [CloneServer new 5556]

#  Run reactor until process interrupted
vwait forever

$server destroy
 
