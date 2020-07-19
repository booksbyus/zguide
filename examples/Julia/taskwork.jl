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
connect(receiver, "tcp://localhost:5557")

# Socket to send messages to
sender = Socket(context, PUSH)
connect(sender, "tcp://localhost:5558")

# Process tasks forever
while true
    s = recv(receiver, String)
    
    # Simple progress indicator for the viewer
    write(stdout, ".")
    flush(stdout)
    
    # Do the work
    sleep(parse(Int, s) * 0.001)

    # Send results to sink
    send(sender, 0x00)
end

close(sender)
close(receiver)
