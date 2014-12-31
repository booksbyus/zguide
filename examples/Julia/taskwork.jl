#!/usr/bin/env julia

#
# Task worker
# Connects PULL socket to tcp://localhost:5557
# Collects workloads from ventilator via that socket
# Connects PUSH socket to tcp://localhost:5558
# Sends results to sink via that socket
#

using ZMQ

context = Context()

# Socket to receive messages on
receiver = Socket(context, PULL)
ZMQ.connect(receiver, "tcp://localhost:5557")

# Socket to send messages to
sender = Socket(context, PUSH)
ZMQ.connect(sender, "tcp://localhost:5558")

# Process tasks forever
while true
    s = bytestring(ZMQ.recv(receiver))
    
    # Simple progress indicator for the viewer
    write(STDOUT, ".")
    flush(STDOUT)
    
    # Do the work
    sleep(int(s)*0.001)

    # Send results to sink
    ZMQ.send(sender, b"")
end