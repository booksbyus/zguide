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
from functools import partial
import zmq
from zmq.eventloop.future import Context
from zmq.eventloop.ioloop import IOLoop
from tornado import gen


SERVER_ADDR = "tcp://127.0.0.1:5555"
#SERVER_ADDR = "tcp://localhost:5555"


@gen.coroutine
def run_server():
    context = Context()
    server = context.socket(zmq.REP)
    server.bind(SERVER_ADDR)
    cycles = 0
    while True:
        request = yield server.recv()
        cycles += 1
        # Simulate various problems, after a few cycles
        if cycles > 3 and randint(0, 3) == 0:
            print("I: Simulating a crash")
            server.unbind(SERVER_ADDR)
            # Delay for a bit, else we get "Address already in use" error.
            # Note that to really simulate a crash, we should probably kill
            # this process and start another.
            yield gen.sleep(2)
            break
        elif cycles > 3 and randint(0, 3) == 0:
            print("I: Simulating CPU overload")
            yield gen.sleep(2)
        print("I: Normal request (%s)" % request)
        yield gen.sleep(1)       # Do some heavy work
        yield server.send(request)
    raise gen.Return((context, server))


@gen.coroutine
def run(loop):
    while True:
        context, server = yield run_server()


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
