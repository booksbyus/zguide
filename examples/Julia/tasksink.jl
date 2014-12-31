#!/usr/bin/env julia

#
# Task sink
# Binds PULL socket to tcp://localhost:5558
# Collects results from workers via that socket
#

using ZMQ

context = Context()

# Socket to receive messages on
receiver = Socket(context, PULL)
ZMQ.bind(receiver, "tcp://*:5558")

# Wait for start of batch
s = ZMQ.recv(receiver)

# Start our tic toc clock
tic()

# Process 100 confirmations
total_msec = 0
for task_nbr in [1:100]
    s = ZMQ.recv(receiver)
    if task_nbr % 10 == 0
        write(STDOUT, ":")
    else
        write(STDOUT, ".")
    end
    flush(STDOUT)
end

# Calculate and report duration of batch
tend = toq()
println("\nTotal elapsed time: $(tend * 1000) msec")

# Making a clean exit.
ZMQ.close(receiver)
ZMQ.close(context)