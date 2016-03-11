#!/usr/bin/env python

"""
synopsis:
    Task worker
    Connects PULL socket to tcp://localhost:5557
    Collects workloads from ventilator via that socket
    Connects PUSH socket to tcp://localhost:5558
    Sends results to sink via that socket
    Author: Lev Givon <lev(at)columbia(dot)edu>
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python taskwork.py
"""

import sys
import zmq
from zmq.asyncio import Context, ZMQEventLoop
import asyncio


@asyncio.coroutine
def run_worker(context):
    # Socket to receive messages on
    receiver = context.socket(zmq.PULL)
    receiver.connect("tcp://localhost:5557")
    # Socket to send messages to
    sender = context.socket(zmq.PUSH)
    sender.connect("tcp://localhost:5558")
    # Process tasks forever
    while True:
        s = yield from receiver.recv()
        # Simple progress indicator for the viewer
        sys.stdout.write('.')
        sys.stdout.flush()
        # Do the work
        yield from asyncio.sleep(int(s) * 0.001)
        # Send results to sink
        yield from sender.send(b'')


@asyncio.coroutine
def run(loop):
    context = Context()
    yield from run_worker(context)


def main():
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    try:
        loop = ZMQEventLoop()
        asyncio.set_event_loop(loop)
        loop.run_until_complete(run(loop))
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')


if __name__ == '__main__':
    main()
