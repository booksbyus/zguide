# Cross-connected ROUTER sockets addressing each other
#
# Author: Lev Givon <lev(at)columbia(dot)edu>

import zmq
import time
import zhelpers

context = zmq.Context()

worker = context.socket(zmq.ROUTER)
worker.setsockopt(zmq.IDENTITY, "WORKER")
worker.bind("ipc://rtrouter.ipc")

server = context.socket(zmq.ROUTER)
server.setsockopt(zmq.IDENTITY, "SERVER")
server.connect("ipc://rtrouter.ipc")

time.sleep(1)

server.send_multipart(["WORKER", "", "send to worker"])
zhelpers.dump(worker)

worker.send_multipart(["SERVER", "", "send to server"])
zhelpers.dump(server)
