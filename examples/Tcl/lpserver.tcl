#
#  Lazy Pirate server
#  Binds REQ socket to tcp://*:5555
#  Like hwserver except:
#   - echoes request as-is
#   - randomly runs slowly, or exits to simulate a crash.
#

package require zmq

expr {srand([pid])}

zmq context context
zmq socket server context REP
server bind "tcp://*:5555"

set cycles 0
while {1} {
    set request [server recv]
    incr cycles

    #  Simulate various problems, after a few cycles
    if {$cycles > 3 && int(rand()*3) == 0} {
	puts "I: simulating a crash"
	break;
    } elseif {$cycles > 3 && int(rand()*3) == 0} {
	puts "I: simulating CPU overload"
	after 2000
    }
    puts "I: normal request ($request)"
    after 1000 ;#  Do some heavy work
    server send $request
}

server close
context term

