#
# Pubsub envelope subscriber
#

package require zmq

# Prepare our context and subscriber
zmq context context
zmq socket subscriber context SUB
subscriber connect "tcp://localhost:5563"
subscriber setsockopt SUBSCRIBE "B"

while {1} {
    # Read envelope with address
    set address [subscriber recv]
    # Read message contents
    set contents [subscriber recv]
    puts "\[$address\] $contents"
}

# We never get here but clean up anyhow
subscriber close
context term
