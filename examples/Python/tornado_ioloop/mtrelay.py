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
from functools import partial
import zmq
from zmq.eventloop.future import Context
from zmq.eventloop.ioloop import IOLoop
from tornado import gen


DEBUG = True


def printdbg(*args):
    if DEBUG:
        print(*args)


@gen.coroutine
def step1(loop, context=None):
    """Step 1"""
    context = context or Context.instance()
    # Signal downstream to step 2
    sender = context.socket(zmq.PAIR)
    sender.connect("inproc://step2")
    msg = b'message from step1'
    sender.send(msg)
    printdbg('(step1) sent msg: {}'.format(msg))


@gen.coroutine
def step2(loop, context=None):
    """Step 2"""
    context = context or Context.instance()
    # Bind to inproc: endpoint, then start upstream thread
    receiver = context.socket(zmq.PAIR)
    receiver.bind("inproc://step2")
    loop.add_callback(partial(step1, loop))
    # Wait for signal
    msg = yield receiver.recv()
    printdbg('(step2) received msg: {}'.format(msg))
    # Signal downstream to step 3
    sender = context.socket(zmq.PAIR)
    sender.connect("inproc://step3")
    msg = b'message from step2'
    yield sender.send(msg)
    printdbg('(step2) sent msg: {}'.format(msg))


@gen.coroutine
def run(loop):
    """ server routine """
    # Prepare our context and sockets
    context = Context.instance()
    # Bind to inproc: endpoint, then start upstream thread
    receiver = context.socket(zmq.PAIR)
    receiver.bind("inproc://step3")
    loop.add_callback(partial(step2, loop))
    # Wait for signal
    msg = yield receiver.recv()
    print("Test successful!  msg: {}".format(msg))
    # Note that terminating the context and closing the socket does not
    #     work with the async aproach.
    #context.term()
    #printdbg('(run) after term')
    #receiver.close()
    #printdbg('(run) after close')


def main():
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    try:
        loop = IOLoop.current()
        loop.run_sync(partial(run, loop))
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')


if __name__ == "__main__":
    main()
