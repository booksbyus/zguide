#
# Custom routing Router to Papa (ROUTER to REP)
#

package require zmq

# We will do this all in one thread to emphasize the sequence
# of events…

zmq context context

zmq socket client context ROUTER
client bind "ipc://routing.ipc"

zmq socket worker context REP
worker setsockopt IDENTITY "A"
worker connect "ipc://routing.ipc"

# Wait for the worker to connect so that when we send a message
# with routing envelope, it will actually match the worker…
after 1000

# Send papa address, address stack, empty part, and request
client sendmore "A"
client sendmore "address 3"
client sendmore "address 2"
client sendmore "address 1"
client sendmore ""
client send     "This is the workload"

# Worker should get just the workload
puts "--------------------------------------------------"
puts [join [worker dump] \n]

# We don't play with envelopes in the worker
worker send "This is the reply"

# Now dump what we got off the ROUTER socket…
puts "--------------------------------------------------"
puts [join [client dump] \n]

client close
worker close
context term

