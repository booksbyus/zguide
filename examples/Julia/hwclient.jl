#!/usr/bin/env julia

#
# Hello World client in Julia
# Connects REQ socket to tcp://localhost:5555
# Sends "Hello" to server, expects "World" back
#

using ZMQ

context = Context()

# Socket to talk to server
println("Connecting to hello world server...")
socket = Socket(context, REQ)
ZMQ.connect(socket, "tcp://localhost:5555")

for request in 1:10
    println("Sending request $request ...")
    ZMQ.send(socket, "Hello")

    # Get the reply.
    message = String(ZMQ.recv(socket))
    println("Received reply $request [ $message ]")
end

# Making a clean exit.
ZMQ.close(socket)
ZMQ.close(context)
