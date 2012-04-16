#
#  Synchronized subscriber
#

package require zmq

zmq context context

# First, connect our subscriber socket
zmq socket subscriber context SUB
subscriber connect "tcp://localhost:5561"
subscriber setsockopt SUBSCRIBE ""

# 0MQ is so fast, we need to wait a while…
after 1000

# Second, synchronize with publisher
zmq socket syncclient context REQ
syncclient connect "tcp://localhost:5562"

# - send a synchronization request
syncclient send ""

# - wait for synchronization reply
syncclient recv

# Third, get our updates and report how many we got
set update_nbr 0
while {1} {
    set string [subscriber recv]
    if {$string eq "END"} {
	break;
    }
    incr update_nbr
}
puts "Received $update_nbr updates"

subscriber close
syncclient close
context term
