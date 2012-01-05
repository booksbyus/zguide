# encoding: utf-8
#
#   Custom routing Router to Dealer
#
#   Author: Jeremy Avnet (brainsik) <spork(dash)zmq(at)theory(dot)org>
#

import time
import random
from threading import Thread

import zmq


# We have two workers, here we copy the code, normally these would
# run on different boxes...
#
def worker_a(context):
    worker = context.socket(zmq.DEALER)
    worker.setsockopt(zmq.IDENTITY, 'A')
    worker.connect("ipc://routing.ipc")

    total = 0
    while True:
        # We receive one part, with the workload
        request = worker.recv()
        finished = request == "END"
        if finished:
            print "A received:", total
            break
        total += 1


def worker_b(context):
    worker = context.socket(zmq.DEALER)
    worker.setsockopt(zmq.IDENTITY, 'B')
    worker.connect("ipc://routing.ipc")

    total = 0
    while True:
        # We receive one part, with the workload
        request = worker.recv()
        finished = request == "END"
        if finished:
            print "B received:", total
            break
        total += 1


context = zmq.Context()
client = context.socket(zmq.ROUTER)
client.bind("ipc://routing.ipc")

Thread(target=worker_a, args=(context,)).start()
Thread(target=worker_b, args=(context,)).start()

# Wait for threads to stabilize
time.sleep(1)

# Send 10 tasks scattered to A twice as often as B
for _ in xrange(10):
    # Send two message parts, first the address...
    if random.randint(0, 2) > 0:
        client.send("A", zmq.SNDMORE)
    else:
        client.send("B", zmq.SNDMORE)

    # And then the workload
    client.send("This is the workload")

client.send("A", zmq.SNDMORE)
client.send("END")

client.send("B", zmq.SNDMORE)
client.send("END")

time.sleep(1)  # Give 0MQ/2.0.x time to flush output
