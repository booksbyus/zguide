#!/usr/bin/env python

"""
synopsis:
    Multithreaded relay.
    Replaces multiple threads with async callbacks.
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python mtrelay.py
"""

import sys
#from functools import partial
import zmq
from zmq.asyncio import Context, ZMQEventLoop
import asyncio


DEBUG = False


def printdbg(*args):
    if DEBUG:
        print(*args)


@asyncio.coroutine
def step1(loop, context):
    """Step 1"""
    context = context or Context.instance()
    # Signal downstream to step 2
    sender = context.socket(zmq.PAIR)
    sender.connect("inproc://step2")
    msg = b'message from step1'
    yield from sender.send(msg)
    print('step1 -- sent msg: {}'.format(msg))


@asyncio.coroutine
def step2(loop, context):
    """Step 2"""
    context = context or Context.instance()
    # Bind to inproc: endpoint, then start upstream thread
    receiver = context.socket(zmq.PAIR)
    receiver.bind("inproc://step2")
    # Wait for signal
    printdbg('(step2) waiting for signal')
    msg = yield from receiver.recv()
    print('step2 -- received msg: {}'.format(msg))
    # Signal downstream to step 3
    sender = context.socket(zmq.PAIR)
    sender.connect("inproc://step3")
    msg = b'message from step2'
    yield from sender.send(msg)
    print('step2 -- sent msg: {}'.format(msg))


@asyncio.coroutine
def step3(loop, context):
    """Collect all the tasks, then wait for them to complete."""
    printdbg('(step3) starting')
    receiver = context.socket(zmq.PAIR)
    receiver.bind("inproc://step3")
    msg = yield from receiver.recv()
    print("step 3 -- test successful!  msg: {}".format(msg))


def run(loop):
    printdbg('(run) starting')
    context = Context()
    tasks = [
        asyncio.ensure_future(step1(loop, context)),
        asyncio.ensure_future(step2(loop, context)),
        asyncio.ensure_future(step3(loop, context)),
    ]
    loop.run_until_complete(asyncio.wait(tasks))


def main():
    """main function"""
    printdbg('(main) starting')
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    try:
        loop = ZMQEventLoop()
        asyncio.set_event_loop(loop)
        printdbg('(main) before starting run()')
        run(loop)
        printdbg('(main) after starting run()')
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')


if __name__ == "__main__":
    #import pdb; pdb.set_trace()
    main()
