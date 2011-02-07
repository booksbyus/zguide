# Publisher for durable subscriber
#
# Author: Lev Givon <lev(at)columbia(dot)edu>

import zmq
import time

context = zmq.Context()

# Subscriber tells us when it's ready here
sync = context.socket(zmq.PULL)
sync.bind("tcp://*:5564")

# We send updates via this socket
publisher = context.socket(zmq.PUB)
publisher.bind("tcp://*:5565")

# Prevent publisher overflow from slow subscribers
publisher.setsockopt(zmq.HWM, 1)

# Specify the swap space in bytes, this covers all subscribers
publisher.setsockopt(zmq.SWAP, 25000000)

# Wait for synchronization request
sync_request = sync.recv()

# Now broadcast exactly 10 updates with pause
for n in xrange(10):
    msg = "Update %d" % n
    publisher.send(msg)
    time.sleep(1)

publisher.send("END")
time.sleep(1)  # Give 0MQ/2.0.x time to flush output
