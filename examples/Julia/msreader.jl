#!/usr/bin/env julia

# Reading from multiple sockets

# The ZMQ.jl wrapper implements ZMQ.recv as a blocking function. Nonblocking i/o
# in Julia is typically done using coroutines (Tasks).
# The @async macro puts its enclosed expression in a Task. When the macro is
# executed, its Task gets scheduled and execution continues immediately to
# whatever follows the macro.

# Note: the msreader example in the zguide is presented as a "dirty hack"
# using the ZMQ_DONTWAIT and EAGAIN codes. Since the ZMQ.jl wrapper API
# does not expose DONTWAIT directly, this example skips the hack and instead
# provides an efficient solution.

using ZMQ

# Prepare our context and sockets
context = ZMQ.Context()

# Connect to task ventilator
receiver = Socket(context, ZMQ.PULL)
ZMQ.connect(receiver, "tcp://localhost:5557")

# Connect to weather server
subscriber = Socket(context,ZMQ.SUB)
ZMQ.connect(subscriber,"tcp://localhost:5556")
ZMQ.set_subscribe(subscriber, "10001")

while true

    # Process any waiting tasks
    @async begin
        msg = unsafe_string(ZMQ.recv(receiver))
        println(msg)
    end

    # Process any waiting weather updates
    @async begin
        msg = unsafe_string(ZMQ.recv(subscriber))
        println(msg)
    end

    # Sleep for 1 msec
    sleep(0.001)
end
