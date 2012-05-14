#
# Broker peering simulation (part 1)
# Prototypes the state flow
#

package require zmq

# First argument is this broker's name
# Other arguments are our peers' names
#
if {[llength $argv] < 1} {
    puts "Usage: peering1.tcl me ?you ...?\n"
    exit 1
}

set self [lindex $argv 0]
puts "I: preparing broker at $self"
expr {srand([pid])}

# Prepare our context and sockets
zmq context context
zmq socket statebe context PUB
statebe bind "ipc://$self-state.ipc"

# Connect statefe to all peers
zmq socket statefe context SUB
statefe setsockopt SUBSCRIBE ""

foreach peer [lrange $argv 1 end] {
    puts "I: connecting to state backend at '$peer'"
    statefe connect "ipc://$peer-state.ipc"
}

# Send out status messages to peers, and collect from peers
#

proc handle_incoming {} {
    set peer_name [statefe recv]
    set available [statefe recv]
    puts "$peer_name - $available workers free"
}

proc send_random {} {
    global self
    set data [expr {int(rand()*10)}]
    statebe sendmore $self
    statebe send $data
    after 1000 send_random
}

statefe readable handle_incoming
send_random

vwait forever

statebe close
statefe close
context term
