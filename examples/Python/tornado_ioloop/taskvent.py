#!/usr/bin/env python

"""
synopsis:
    Task ventilator
    Binds PUSH socket to tcp://localhost:5557
    Sends batch of tasks to workers via that socket
    Author: Lev Givon <lev(at)columbia(dot)edu>
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python taskvent.py
"""

import sys
from functools import partial
import zmq
import random
from zmq.eventloop.future import Context
from zmq.eventloop.ioloop import IOLoop
from tornado import gen

try:
    raw_input
except NameError:
    # Python 3
    raw_input = input


@gen.coroutine
def run_ventilator(context):
    # Socket to send messages on
    sender = context.socket(zmq.PUSH)
    sender.bind("tcp://*:5557")
    # Socket with direct access to the sink: used to synchronize start of batch
    sink = context.socket(zmq.PUSH)
    sink.connect("tcp://localhost:5558")
    print("Press Enter when the workers are ready: ")
    raw_input()
    print("Sending tasks to workers...")
    # The first message is "0" and signals start of batch
    yield sink.send(b'0')
    # Initialize random number generator
    random.seed()
    # Send 100 tasks
    total_msec = 0
    for task_nbr in range(100):
        # Random workload from 1 to 100 msecs
        workload = random.randint(1, 100)
        total_msec += workload
        yield sender.send_string(u'%i' % workload)
    print("Total expected cost: %s msec" % total_msec)
    # Give 0MQ time to deliver
    yield gen.sleep(1)


@gen.coroutine
def run(loop):
    context = Context()
    yield run_ventilator(context)


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
