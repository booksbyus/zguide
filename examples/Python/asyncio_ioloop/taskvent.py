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
import zmq
import random
from zmq.asyncio import Context, ZMQEventLoop
import asyncio

try:
    raw_input
except NameError:
    # Python 3
    raw_input = input


@asyncio.coroutine
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
    yield from sink.send(b'0')
    # Initialize random number generator
    random.seed()
    # Send 100 tasks
    total_msec = 0
    for task_nbr in range(100):
        # Random workload from 1 to 100 msecs
        workload = random.randint(1, 100)
        total_msec += workload
        yield from sender.send_string(u'%i' % workload)
    print("Total expected cost: %s msec" % total_msec)
    # Give 0MQ time to deliver
    yield from asyncio.sleep(1)


@asyncio.coroutine
def run(loop):
    context = Context()
    yield from run_ventilator(context)


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
