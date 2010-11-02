# encoding: utf-8
#
#   Publisher for durable subscriber
#
#   Author: Jeremy Avnet (brainsik) <spork(dash)zmq(at)theory(dot)org>
#

import zmq
import time

context = zmq.Context()

# Subscriber tells us when it's ready here
sync = context.socket(zmq.PULL)
sync.bind("tcp://*:5564")

# We send updates via this socket
publisher = context.socket(zmq.PUB)
publisher.bind("tcp://*:5565")

# Wait for synchronization request
sync_request = sync.recv()

# Now broadcast exactly 10 updates with pause
for n in xrange(10):
    msg = "Update %d" % n
    publisher.send(msg)
    time.sleep(1)

publisher.send("END")
time.sleep(1)  # Give 0MQ/2.0.x time to flush output
