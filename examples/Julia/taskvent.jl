#!/usr/bin/env julia

#
# Task ventilator
# Binds PUSH socket to tcp://localhost:5557
# Sends batch of tasks to workers via that socket
#

using ZMQ
using Random: seed!

context = Context()
sender = Socket(context, PUSH)
bind(sender, "tcp://*:5557")

# Socket with direct access to the sink: used to synchronize start of batch
sink = Socket(context, PUSH)
connect(sink, "tcp://localhost:5558")

println("Press Enter when the workers are ready: ")
_ = readline(stdin)
println("Sending tasks to workers...")

# The first message is "0" and signals start of batch
send(sink, 0x30)

# Initialize random number generator
seed!(1)

# Send 100 tasks
total_msec = 0
for task_nbr in 1:100
    global total_msec
    # Random workload from 1 to 100 msecs
    workload = rand(1:100)
    total_msec += workload
    send(sender, "$workload")
end

println("Total expected cost: $total_msec msec")

# Give 0MQ time to deliver
sleep(1)

# Making a clean exit.
close(sender)
close(sink)
close(context)
