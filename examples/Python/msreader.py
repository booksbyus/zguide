#
# Reading from multiple sockets
# This version uses a simple recv loop
#
# Author: Kamil Kisiel <kamil@kamilkisiel.net>
#
import zmq
import time

# Prepare our context and sockets
context = zmq.Context()

# Connect to task ventilator
receiver = context.socket(zmq.PULL)
receiver.connect("tcp://localhost:5557")

# Connect to weather server
subscriber = context.socket(zmq.SUB)
subscriber.connect("tcp://localhost:5556")
subscriber.setsockopt(zmq.SUBSCRIBE, "10001")

# Process messages from both sockets
# We prioritize traffic from the task ventilator
while True:
    # Process waiting tasks
    try:
        while True:
            task = receiver.recv(flags=zmq.NOBLOCK)
            # Process task
    except:
        pass

    # Process waiting weather updates
    try:
        while True:
            update = subscriber.recv(flags=zmq.NOBLOCK)
            # Process weather update
    except:
        pass

    # No activity, so sleep for 1 msec
    time.sleep(0.001)

# We never get here but clean up anyhow
receiver.close()
subscriber.close()
context.term()
