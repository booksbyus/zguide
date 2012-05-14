#
#  Simple Pirate queue
#  This is identical to the LRU pattern, with no reliability mechanisms
#  at all. It depends on the client for recovery. Runs forever.
#
#   Author: Daniel Lundin <dln(at)eintr(dot)org>
#

import zmq

LRU_READY = "\x01"

context = zmq.Context(1)

frontend = context.socket(zmq.ROUTER) # ROUTER
backend = context.socket(zmq.ROUTER) # ROUTER
frontend.bind("tcp://*:5555") # For clients
backend.bind("tcp://*:5556")  # For workers

poll_workers = zmq.Poller()
poll_workers.register(backend, zmq.POLLIN)

poll_both = zmq.Poller()
poll_both.register(frontend, zmq.POLLIN)
poll_both.register(backend, zmq.POLLIN)

workers = []

while True:
    if workers:
        socks = dict(poll_both.poll())
    else:
        socks = dict(poll_workers.poll())

    # Handle worker activity on backend
    if socks.get(backend) == zmq.POLLIN:
        # Use worker address for LRU routing
        msg = backend.recv_multipart()
        if not msg:
            break
        address = msg[0]
        workers.append(address)

        # Everything after the second (delimiter) frame is reply
        reply = msg[2:]

        # Forward message to client if it's not a READY
        if reply[0] != LRU_READY:
            frontend.send_multipart(reply)

    if socks.get(frontend) == zmq.POLLIN:
        #  Get client request, route to first available worker
        msg = frontend.recv_multipart()
        request = [workers.pop(0), ''] + msg
        backend.send_multipart(request)

