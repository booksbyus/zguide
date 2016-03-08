#!/usr/bin/env python

"""
synopsis:
    Simple request-reply broker.
    Author: Lev Givon <lev(at)columbia(dot)edu>
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python rrbroker.py
notes:
    To run this, start rrbroker.py, any number of instances of rrworker.py,
    and rrclient.py.
"""

import sys
import zmq
from zmq.asyncio import Context, Poller, ZMQEventLoop
import asyncio


@asyncio.coroutine
def run_broker(context):
    # Prepare our context and sockets
    frontend = context.socket(zmq.ROUTER)
    backend = context.socket(zmq.DEALER)
    frontend.bind("tcp://*:5559")
    backend.bind("tcp://*:5560")
    # Initialize poll set
    poller = Poller()
    poller.register(frontend, zmq.POLLIN)
    poller.register(backend, zmq.POLLIN)
    # Switch messages between sockets
    while True:
        socks = yield from poller.poll()
        socks = dict(socks)
        if socks.get(frontend) == zmq.POLLIN:
            message = yield from frontend.recv_multipart()
            print('received from frontend: {}'.format(message))
            yield from backend.send_multipart(message)
        if socks.get(backend) == zmq.POLLIN:
            message = yield from backend.recv_multipart()
            print('received from backend: {}'.format(message))
            yield from frontend.send_multipart(message)


@asyncio.coroutine
def run(loop):
    context = Context()
    yield from run_broker(context)


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
