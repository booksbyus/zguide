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
from functools import partial
from random import randrange
import zmq
from zmq.eventloop.future import Context
from zmq.eventloop.ioloop import IOLoop
#from tornado import gen

CLIENT_ADDRESS = "tcp://*:5556"


# @gen.coroutine
async def run_server(context):
    socket = context.socket(zmq.PUB)
    socket.bind(CLIENT_ADDRESS)
    while True:
        zipcode = randrange(10000, 10010)
        temperature = randrange(-80, 135)
        relhumidity = randrange(10, 60)
        msg = "%i %i %i" % (
            zipcode, temperature, relhumidity)
        msg = msg.encode('utf-8')
        await socket.send(msg)


# @gen.coroutine
async def run(loop):
    context = Context()
    while True:
        await run_server(context)


def main():
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    try:
        loop = IOLoop.current()
        loop.run_sync(partial(run, loop))
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')


if __name__ == '__main__':
    main()
