#!/usr/bin/env python

"""
synopsis:
    Weather update server
    Binds PUB socket to tcp://*:5556
    Publishes random weather updates
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python wuserver.py
"""

import sys
from random import randrange
import zmq
from zmq.asyncio import Context, ZMQEventLoop
import asyncio


CLIENT_ADDRESS = "tcp://*:5556"


@asyncio.coroutine
def run_server(context):
    socket = context.socket(zmq.PUB)
    socket.bind(CLIENT_ADDRESS)
    while True:
        zipcode = randrange(10000, 10010)
        temperature = randrange(-80, 135)
        relhumidity = randrange(10, 60)
        msg = "%i %i %i" % (
            zipcode, temperature, relhumidity)
        msg = msg.encode('utf-8')
        yield from socket.send(msg)


@asyncio.coroutine
def run(loop):
    context = Context()
    while True:
        yield from run_server(context)


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
    #import pdb; pdb.set_trace()
    main()
