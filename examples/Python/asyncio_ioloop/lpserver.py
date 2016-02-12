#!/usr/bin/env python

"""
synopsis:
    Lazy Pirate server
    Binds REQ socket to tcp://*:5555
    Like hwserver except:
     - echoes request as-is
     - randomly runs slowly
     - randomly exits to simulate a crash.
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python lpserver.py
"""


import sys
from random import randint
import zmq
from zmq.asyncio import Context, ZMQEventLoop
import asyncio


SERVER_ADDR = "tcp://127.0.0.1:5555"
#SERVER_ADDR = "tcp://localhost:5555"


@asyncio.coroutine
def run_server():
    context = Context()
    server = context.socket(zmq.REP)
    server.bind(SERVER_ADDR)
    cycles = 0
    while True:
        request = yield from server.recv()
        cycles += 1
        # Simulate various problems, after a few cycles
        if cycles > 3 and randint(0, 3) == 0:
            print("I: Simulating a crash")
            server.unbind(SERVER_ADDR)
            # Delay for a bit, else we get "Address already in use" error.
            # Note that to really simulate a crash, we should probably kill
            # this process and start another.
            yield from asyncio.sleep(2)
            break
        elif cycles > 3 and randint(0, 3) == 0:
            print("I: Simulating CPU overload")
            yield from asyncio.sleep(2)
        print("I: Normal request (%s)" % request)
        yield from asyncio.sleep(1)       # Do some heavy work
        yield from server.send(request)
    return (context, server)


@asyncio.coroutine
def run(loop):
    while True:
        context, server = yield from run_server()


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
