#
# Demonstrate identities as used by the request-reply pattern.  Run this
# program by itself.
#

package require zmq

zmq context context

zmq socket sink context ROUTER
sink bind "inproc://example"

# First allow 0MQ to set the identity
zmq socket anonymous context REQ
anonymous connect "inproc://example"
anonymous send "ROUTER uses a generated UUID"
puts "--------------------------------------------------"
puts [join [sink dump] \n]

# Then set the identity ourself
zmq socket identified context REQ
identified setsockopt IDENTITY "Hello"
identified connect "inproc://example"
identified send "ROUTER socket uses REQ's socket identity"
puts "--------------------------------------------------"
puts [join [sink dump] \n]

sink close
anonymous close
identified close
context term

