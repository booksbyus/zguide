#
# Reading from multiple sockets
# This version uses socket.poll()
#
# Author: Kamil Kisiel <kamil@kamilkisiel.net>
#
import zmq

context = zmq.Context()

# Connect to task ventilator
receiver = context.socket(zmq.PULL)
receiver.connect("tcp://localhost:5557")

# Connect to weather server
subscriber = context.socket(zmq.SUB)
subscriber.connect("tcp://localhost:5556")
subscriber.setsockopt(zmq.SUBSCRIBE, "10001")

# Initialize the poll set
items = zmq.Poller()
items.register(receiver, flags=zmq.POLLIN)
items.register(subscriber, flags=zmq.POLLIN)

# Process messages from both sockets
while True:
    for item in items.poll(timeout=None):
        if item[0] == receiver and item[1] & zmq.POLLIN:
            message = receiver.recv()
            # Process task
        if item[0] == subscriber and item[1] & zmq.POLLIN:
            message = subscriber.recv()
            # Process weather update

# We never get here
receiver.close()
subscriber.close()
context.term()
