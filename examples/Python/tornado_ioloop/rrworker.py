#!/usr/bin/env python

"""
synopsis:
    Request-reply service in Python
    Connects REP socket to tcp://localhost:5560
    Expects "Hello" from client, replies with "World"
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python rrworker.py
notes:
    To run this, start rrbroker.py, any number of instances of rrworker.py,
    and rrclient.py.
"""

import sys
import zmq
from functools import partial
from zmq.eventloop.future import Context
from zmq.eventloop.ioloop import IOLoop
from tornado import gen


@gen.coroutine
def run_worker(context):
    socket = context.socket(zmq.REP)
    socket.connect("tcp://localhost:5560")
    while True:
        message = yield socket.recv()
        print("Received request: %s" % message)
        message = message.decode('utf-8')
        reply = '{}, World'.format(message)
        reply = reply.encode('utf-8')
        yield socket.send(reply)
        print("Sent reply: {}".format(reply))


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
