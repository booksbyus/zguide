#!/usr/bin/env julia

#
#   Weather update client
#   Connects SUB socket to tcp://localhost:5556
#   Collects weather updates and finds avg temp in zipcode
#

using ZMQ

context = Context()
socket = Socket(context, SUB)

println("Collecting updates from weather server...")
connect(socket, "tcp://localhost:5556")

# Subscribe to zipcode, default is NYC, 10001
zip_filter = length(ARGS) > 0 ? int(ARGS[1]) : 10001

subscribe(socket, string(zip_filter))

# Process 5 updates
update_nbr = 5

total_temp = 0
for update in 1:update_nbr
    global total_temp
    message = unsafe_string(recv(socket))
    zipcode, temperature, relhumidity = split(message)
    total_temp += parse(Int, temperature)
end

avg_temp = total_temp / update_nbr

println("Average temperature for zipcode $zip_filter was $(avg_temp)F")

# Making a clean exit.
close(socket)
close(context)
