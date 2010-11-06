# encoding: utf-8
#
#   Durable subscriber
#
#   Author: Jeremy Avnet (brainsik) <spork(dash)zmq(at)theory(dot)org>
#

import zmq
import time

context = zmq.Context()

# Connect our subscriber socket
subscriber = context.socket(zmq.SUB)
subscriber.setsockopt(zmq.IDENTITY, "Hello")
subscriber.setsockopt(zmq.SUBSCRIBE, "")
subscriber.connect("tcp://localhost:5565")

# Syncronize with the publisher
sync = context.socket(zmq.PUSH)
sync.connect("tcp://localhost:5564")
sync.send("")

# Get updates, expect random Ctrl-C death
while True:
    data = subscriber.recv()
    print data
    if data == "END":
        break
