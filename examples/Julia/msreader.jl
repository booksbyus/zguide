import ZMQ

# Prepare our context and sockets
context = ZMQ.Context()

# Connect to task ventilator
receiver = Socket(context, ZMQ.PULL)
ZMQ.connect(receiver, "tcp://localhost:5557")

# Connect to weather server
subscriber = Socket(context,ZMQ.SUB)
ZMQ.connect(subscriber,"tcp://localhost:5556")
ZMQ.set_subscribe(subscriber, "10001")

# Process messages from both sockets
# We prioritize traffic from the task ventilator


# this is a literal translation, which is wrong.
# Trying to figure out how to get nonblocking behavior from recv
# also poll is not yet implemented?
while true

    # Process any waiting tasks
    while true
        try
            msg = receiver.recv(zmq.DONTWAIT)
        catch er
            if isa(er,zmq.Again)
                break
            end
            # process task
        end
    end

    # Process any waiting weather updates
    while true
        try
            msg = subscriber.recv(zmq.DONTWAIT)
        catch er
            if isa(er,zmq.Again)
                break
            end
        end
    end
    # process weather update

    # No activity, so sleep for 1 msec
    time.sleep(0.001)
    
end

