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
from functools import partial
import zmq
from zmq.eventloop.future import Context, Poller
from zmq.eventloop.ioloop import IOLoop
from tornado import gen


@gen.coroutine
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
        socks = yield poller.poll()
        socks = dict(socks)
        if socks.get(frontend) == zmq.POLLIN:
            message = yield frontend.recv_multipart()
            print('received from frontend: {}'.format(message))
            yield backend.send_multipart(message)
        if socks.get(backend) == zmq.POLLIN:
            message = yield backend.recv_multipart()
            print('received from backend: {}'.format(message))
            yield frontend.send_multipart(message)


@gen.coroutine
def run(loop):
    context = Context()
    yield run_broker(context)


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
