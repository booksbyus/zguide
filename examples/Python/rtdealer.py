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
def worker_a(context=None):
    context = context or zmq.Context.instance()
    worker = context.socket(zmq.DEALER)
    worker.setsockopt(zmq.IDENTITY, b'A')
    worker.connect("ipc://routing.ipc")

    total = 0
    while True:
        # We receive one part, with the workload
        request = worker.recv()
        finished = request == b"END"
        if finished:
            print("A received: %s" % total)
            break
        total += 1


def worker_b(context=None):
    context = context or zmq.Context.instance()
    worker = context.socket(zmq.DEALER)
    worker.setsockopt(zmq.IDENTITY, b'B')
    worker.connect("ipc://routing.ipc")

    total = 0
    while True:
        # We receive one part, with the workload
        request = worker.recv()
        finished = request == b"END"
        if finished:
            print("B received: %s" % total)
            break
        total += 1


context = zmq.Context.instance()
client = context.socket(zmq.ROUTER)
client.bind("ipc://routing.ipc")

Thread(target=worker_a).start()
Thread(target=worker_b).start()

# Wait for threads to stabilize
time.sleep(1)

# Send 10 tasks scattered to A twice as often as B
for _ in range(10):
    # Send two message parts, first the address...
    ident = random.choice([b'A', b'A', b'B'])
    # And then the workload
    work = b"This is the workload"
    client.send_multipart([ident, work])

client.send_multipart([b'A', b'END'])
client.send_multipart([b'B', b'END'])
