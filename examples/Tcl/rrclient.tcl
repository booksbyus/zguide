#
# Hello World client
# Connects REQ socket to tcp://localhost:5559
# Sends "Hello" to server, expects "World" back
#

package require zmq

zmq context context

# Socket to talk to server
zmq socket requester context REQ
requester connect "tcp://localhost:5559"

for {set request_nbr 0} {$request_nbr < 10} { incr request_nbr} {
    requester send "Hello"
    set string [requester recv]
    puts "Received reply $request_nbr \[$string\]"
}

requester close
context term
