#!/usr/bin/env python

"""
synopsis:
    Custom routing Router to Dealer
    Author: Jeremy Avnet (brainsik) <spork(dash)zmq(at)theory(dot)org>
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python rtdealer.py
"""

import sys
import random
import zmq
from zmq.asyncio import Context, ZMQEventLoop
import asyncio

CONNECTION_ADDRESS = "ipc://routing.ipc"


# We have two workers, here we copy the code, normally these would
# run on different boxes...
#
@asyncio.coroutine
def worker_a(context=None):
    context = context or zmq.Context.instance()
    worker = context.socket(zmq.DEALER)
    worker.setsockopt(zmq.IDENTITY, b'A')
    worker.connect(CONNECTION_ADDRESS)
    total = 0
    while True:
        # We receive one part, with the workload
        request = yield from worker.recv()
        finished = request == b"END"
        if finished:
            print("A received: %s" % total)
            break
        total += 1
    return ('worker_a', total)


@asyncio.coroutine
def worker_b(context=None):
    context = context or zmq.Context.instance()
    worker = context.socket(zmq.DEALER)
    worker.setsockopt(zmq.IDENTITY, b'B')
    worker.connect(CONNECTION_ADDRESS)
    total = 0
    while True:
        # We receive one part, with the workload
        request = yield from worker.recv()
        finished = request == b"END"
        if finished:
            print("B received: %s" % total)
            break
        total += 1
    return ('worker_b', total)


@asyncio.coroutine
def dealer(client):
    print('(dealer) starting')
    # Send 10 tasks scattered to A twice as often as B
    for _ in range(10):
        # Send two message parts, first the address,
        # and then the workload.
        ident = random.choice([b'A', b'A', b'B'])
        work = b"This is the workload"
        yield from client.send_multipart([ident, work])
    yield from client.send_multipart([b'A', b'END'])
    yield from client.send_multipart([b'B', b'END'])
    return ('dealer', 'finished')


def run(loop):
    context = Context.instance()
    client = context.socket(zmq.ROUTER)
    client.bind(CONNECTION_ADDRESS)
    tasks = [
        asyncio.ensure_future(worker_a(context)),
        asyncio.ensure_future(worker_b(context)),
        asyncio.ensure_future(dealer(client)),
    ]
    loop.run_until_complete(asyncio.wait(tasks))
    for task in tasks:
        print('result: {}'.format(task.result()))


def main():
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    try:
        loop = ZMQEventLoop()
        asyncio.set_event_loop(loop)
        run(loop)
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')
        sys.exit(0)


if __name__ == '__main__':
    main()
    print('(program) finished')
