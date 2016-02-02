#!/usr/bin/env python

"""
synopsis:
    Paranoid Pirate queue
    Original author: Daniel Lundin <dln(at)eintr(dot)org>
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python ppqueue.py
"""

import sys
from collections import OrderedDict
import time
from functools import partial
import zmq
from zmq.eventloop.future import Context, Poller
from zmq.eventloop.ioloop import IOLoop
from tornado import gen

HEARTBEAT_LIVENESS = 3     # 3..5 is reasonable
HEARTBEAT_INTERVAL = 1.0   # Seconds

#  Paranoid Pirate Protocol constants
PPP_READY = b"\x01"      # Signals worker is ready
PPP_HEARTBEAT = b"\x02"  # Signals worker heartbeat
FRONT_END_ADDRESS = 'tcp://*:5555'
BACK_END_ADDRESS = 'tcp://*:5556'


class Worker(object):
    def __init__(self, address):
        self.address = address
        self.expiry = time.time() + HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS


class WorkerQueue(object):
    def __init__(self):
        self.queue = OrderedDict()

    def ready(self, worker):
        self.queue.pop(worker.address, None)
        self.queue[worker.address] = worker

    def purge(self):
        """Look for & kill expired workers."""
        t = time.time()
        expired = []
        for address, worker in self.queue.items():
            if t > worker.expiry:  # Worker expired
                expired.append(address)
        for address in expired:
            print("W: Idle worker expired: %s" % address)
            self.queue.pop(address, None)

    def __next__(self):
        address, worker = self.queue.popitem(False)
        return address


@gen.coroutine
def run_queue(context):
    frontend = context.socket(zmq.ROUTER)    # ROUTER
    backend = context.socket(zmq.ROUTER)     # ROUTER
    frontend.bind(FRONT_END_ADDRESS)    # For clients
    backend.bind(BACK_END_ADDRESS)      # For workers
    poll_workers = Poller()
    poll_workers.register(backend, zmq.POLLIN)
    poll_both = Poller()
    poll_both.register(frontend, zmq.POLLIN)
    poll_both.register(backend, zmq.POLLIN)
    workers = WorkerQueue()
    heartbeat_at = time.time() + HEARTBEAT_INTERVAL
    while True:
        if len(workers.queue) > 0:
            poller = poll_both
        else:
            poller = poll_workers
        socks = yield poller.poll(HEARTBEAT_INTERVAL * 1000)
        socks = dict(socks)
        # Handle worker activity on backend
        if socks.get(backend) == zmq.POLLIN:
            # Use worker address for LRU routing
            frames = yield backend.recv_multipart()
            if not frames:
                break
            address = frames[0]
            workers.ready(Worker(address))
            # Validate control message, or return reply to client
            msg = frames[1:]
            if len(msg) == 1:
                if msg[0] not in (PPP_READY, PPP_HEARTBEAT):
                    print("E: Invalid message from worker: %s" % msg)
            else:
                yield frontend.send_multipart(msg)
            # Send heartbeats to idle workers if it's time
            if time.time() >= heartbeat_at:
                for worker in workers.queue:
                    msg = [worker, PPP_HEARTBEAT]
                    yield backend.send_multipart(msg)
                heartbeat_at = time.time() + HEARTBEAT_INTERVAL
        if socks.get(frontend) == zmq.POLLIN:
            frames = yield frontend.recv_multipart()
            if not frames:
                break
            frames.insert(0, next(workers))
            backend.send_multipart(frames)
        workers.purge()


@gen.coroutine
def run(loop):
    context = Context()
    while True:
        yield run_queue(context)


def main():
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    try:
        loop = IOLoop.current()
        loop.run_sync(partial(run, loop))
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')


if __name__ == '__main__':
    main()
