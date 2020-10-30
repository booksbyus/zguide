#!/usr/bin/env julia

#
# Hello World server in Julia
# Binds REP socket to tcp://*:5555
# Expects "Hello" from client, replies "World"
#

using ZMQ

context = Context()
socket = Socket(context, REP)
ZMQ.bind(socket, "tcp://*:5555")

while true
    # Wait for next request from client
    message = String(ZMQ.recv(socket))
    println("Received request: $message")

    # Do some 'work'
    sleep(1)

    # Send reply back to client
    ZMQ.send(socket, "World")
end

# classy hit men always clean up when finish the job.
ZMQ.close(socket)
ZMQ.close(context)
