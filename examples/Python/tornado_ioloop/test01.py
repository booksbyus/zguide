#!/usr/bin/env python

"""
synopsis:
    Tests for pyzmq and tornado.
"""


import sys
from functools import partial
import zmq
from zmq.eventloop.future import Context, Poller
from zmq.eventloop.ioloop import IOLoop
from tornado import gen


Ctx = Context()


@gen.coroutine
def func1(arg1):
    # Launch pool of worker threads
    print('(func1) starting.  arg1: {}'.format(arg1))
    result = arg1 * 3
    raise gen.Return(result)


@gen.coroutine
def run1(arg1):
    # Launch pool of worker threads
    print('(run1) starting.  arg1: {}'.format(arg1))
    result = yield func1(arg1)
    print('(run1) result: {}'.format(result))
    raise gen.Return('nothing')


@gen.coroutine
def run2(arg1, loop):
    # Launch pool of worker threads
    print('(run2) starting.  arg1: {}'.format(arg1))
    result = yield func1(arg1)
    print('(run2) result: {}'.format(result))
    # Stop the I/O loop.  The first callback that does this stops the loop.
    loop.stop()


def main1():
    """Server routine"""
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    loop = IOLoop.current()
    #loop.run_sync(lambda: run1('a string'))
    loop.run_sync(partial(run1, 'a string'))


def main2():
    """Server routine"""
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    loop = IOLoop.current()
    loop.add_callback(partial(run2, 'another string', loop))
    loop.start()
    print('(main2) after loop.start')


if __name__ == "__main__":
    #import ipdb; ipdb.set_trace()
    main2()
