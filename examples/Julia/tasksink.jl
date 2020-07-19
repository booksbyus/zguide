#!/usr/bin/env julia

#
# Task sink
# Binds PULL socket to tcp://localhost:5558
# Collects results from workers via that socket
#

using ZMQ
using Dates

context = Context()

# Socket to receive messages on
receiver = Socket(context, PULL)
bind(receiver, "tcp://*:5558")

# Wait for start of batch
s = recv(receiver)

# Start our tic toc clock
tstart = now()

# Process 100 confirmations
for task_nbr in 1:100
    s = recv(receiver)
    if task_nbr % 10 == 0
        write(stdout, ":")
    else
        write(stdout, ".")
    end
    flush(stdout)
end

# Calculate and report duration of batch
tend = now()
elapsed = tend - tstart
println("\nTotal elapsed time: $(elapsed * 1000) msec")

# Making a clean exit.
close(receiver)
close(context)