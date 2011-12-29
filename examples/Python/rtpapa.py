# encoding: utf-8
#
#   Custom routing Router to Papa (ROUTER to REP)
#
#   Author: Jeremy Avnet (brainsik) <spork(dash)zmq(at)theory(dot)org>
#

import time

import zmq

import zhelpers


context = zmq.Context()
client = context.socket(zmq.ROUTER)
client.bind("ipc://routing.ipc")

worker = context.socket(zmq.REP)
worker.setsockopt(zmq.IDENTITY, "A")
worker.connect("ipc://routing.ipc")

# Wait for sockets to stabilize
time.sleep(1)

client.send("A", zmq.SNDMORE)
client.send("address 3", zmq.SNDMORE)
client.send("address 2", zmq.SNDMORE)
client.send("address 1", zmq.SNDMORE)
client.send("", zmq.SNDMORE)
client.send("This is the workload")

# Worker should get just the workload
zhelpers.dump(worker)

# We don't play with envelopes in the worker
worker.send("This is the reply")

# Now dump what we got off the ROUTER socket...
zhelpers.dump(client)
