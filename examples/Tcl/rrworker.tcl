#
#  Hello World server
#  Connects REP socket to tcp://*:5560
#  Expects "Hello" from client, replies with "World"
#

package require zmq

zmq context context

# Socket to talk to clients
zmq socket responder context REP
responder connect "tcp://localhost:5560"

while {1} {
    # Wait for next request from client
    set string [responder recv]
    puts "Received request: \[$string\]"

    # Do some 'work'
    after 1000;

    # Send reply back to client
    responder send "World"
}

# We never get here but clean up anyhow
responder close
context term

