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
from zmq.asyncio import Context, ZMQEventLoop
import asyncio


@asyncio.coroutine
def run_worker(context):
    socket = context.socket(zmq.REP)
    socket.connect("tcp://localhost:5560")
    while True:
        message = yield from socket.recv()
        print("Received request: %s" % message)
        message = message.decode('utf-8')
        reply = '{}, World'.format(message)
        reply = reply.encode('utf-8')
        yield from socket.send(reply)
        print("Sent reply: {}".format(reply))


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
