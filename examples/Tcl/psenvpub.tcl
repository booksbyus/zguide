#
# Pubsub envelope publisher
# Note that the zhelpers.h file also provides sendmore
#

package require zmq

# Prepare our context and publisher
zmq context context
zmq socket publisher context PUB
publisher bind "tcp://*:5563"

while {1} {
    # Write two messages, each with an envelope and content
    publisher sendmore "A"
    publisher send "We don't want to see this"
    publisher sendmore "B"
    publisher send "We would like to see this"
    after 1000
}

# We never get here but clean up anyhow
publisher close
context term
