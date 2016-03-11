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
from functools import partial
import zmq
from zmq.eventloop.future import Context
from zmq.eventloop.ioloop import IOLoop
from tornado import gen


@gen.coroutine
def run_worker(context):
    # Socket to receive messages on
    receiver = context.socket(zmq.PULL)
    receiver.connect("tcp://localhost:5557")
    # Socket to send messages to
    sender = context.socket(zmq.PUSH)
    sender.connect("tcp://localhost:5558")
    # Process tasks forever
    while True:
        s = yield receiver.recv()
        # Simple progress indicator for the viewer
        sys.stdout.write('.')
        sys.stdout.flush()
        # Do the work
        yield gen.sleep(int(s) * 0.001)
        # Send results to sink
        yield sender.send(b'')


@gen.coroutine
def run(loop):
    context = Context()
    yield run_worker(context)


def main():
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    try:
        loop = IOLoop.current()
        loop.run_sync(partial(run, loop, ))
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')


if __name__ == '__main__':
    main()
