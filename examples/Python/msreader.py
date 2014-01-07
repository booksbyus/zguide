# encoding: utf-8
#
#   Reading from multiple sockets
#   This version uses a simple recv loop
#
#   Author: Jeremy Avnet (brainsik) <spork(dash)zmq(at)theory(dot)org>
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
subscriber.setsockopt(zmq.SUBSCRIBE, b"10001")

# Process messages from both sockets
# We prioritize traffic from the task ventilator
while True:

    # Process any waiting tasks
    while True:
        try:
            msg = receiver.recv(zmq.DONTWAIT)
        except zmq.Again:
            break
        # process task

    # Process any waiting weather updates
    while True:
        try:
            msg = subscriber.recv(zmq.DONTWAIT)
        except zmq.Again:
            break
        # process weather update

    # No activity, so sleep for 1 msec
    time.sleep(0.001)
