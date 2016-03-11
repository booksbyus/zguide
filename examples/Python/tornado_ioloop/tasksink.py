#!/usr/bin/env python

"""
synopsis:
    Task sink
    Binds PULL socket to tcp://localhost:5558
    Collects results from workers via that socket
    Author: Lev Givon <lev(at)columbia(dot)edu>
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python tasksink.py
"""

import sys
from functools import partial
import time
import zmq
from zmq.eventloop.future import Context
from zmq.eventloop.ioloop import IOLoop
from tornado import gen


@gen.coroutine
def run_sink(context):
    # Socket to receive messages on
    receiver = context.socket(zmq.PULL)
    receiver.bind("tcp://*:5558")
    # Wait for start of batch
    yield receiver.recv()
    # Start our clock now
    tstart = time.time()
    # Process 100 confirmations
    for task_nbr in range(100):
        yield receiver.recv()
        if task_nbr % 10 == 0:
            sys.stdout.write(':')
        else:
            sys.stdout.write('.')
        sys.stdout.flush()
    # Calculate and report duration of batch
    tend = time.time()
    print("Total elapsed time: %d msec" % ((tend - tstart) * 1000))


@gen.coroutine
def run(loop):
    context = Context()
    yield run_sink(context)


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
