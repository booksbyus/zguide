#!/usr/bin/env python

"""
synopsis:
    Custom routing Router to Dealer
    Author: Jeremy Avnet (brainsik) <spork(dash)zmq(at)theory(dot)org>
    Modified for tornado/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python rtdealer.py
"""

import sys
import random
import zmq
from functools import partial
from zmq.eventloop.future import Context
from zmq.eventloop.ioloop import IOLoop
from tornado import gen


# We have two workers, here we copy the code, normally these would
# run on different boxes...
#
@gen.coroutine
def worker_a(context=None):
    context = context or zmq.Context.instance()
    worker = context.socket(zmq.DEALER)
    worker.setsockopt(zmq.IDENTITY, b'A')
    worker.connect("ipc://routing.ipc")
    total = 0
    while True:
        # We receive one part, with the workload
        request = yield worker.recv()
        finished = request == b"END"
        if finished:
            print("A received: %s" % total)
            break
        total += 1
    print('(worker_a) finished')
    raise gen.Return(('worker_a', total))


@gen.coroutine
def worker_b(context=None):
    context = context or zmq.Context.instance()
    worker = context.socket(zmq.DEALER)
    worker.setsockopt(zmq.IDENTITY, b'B')
    worker.connect("ipc://routing.ipc")
    total = 0
    while True:
        # We receive one part, with the workload
        request = yield worker.recv()
        finished = request == b"END"
        if finished:
            print("B received: %s" % total)
            break
        total += 1
    print('(worker_b) finished')
    raise gen.Return(('worker_b', total))


@gen.coroutine
def dealer(client):
    print('(dealer) starting')
    # Send 10 tasks scattered to A twice as often as B
    for _ in range(10):
        # Send two message parts, first the address,
        # and then the workload.
        ident = random.choice([b'A', b'A', b'B'])
        work = b"This is the workload"
        yield client.send_multipart([ident, work])
    yield client.send_multipart([b'A', b'END'])
    yield client.send_multipart([b'B', b'END'])
    print('(dealer) finished')
    raise gen.Return(('dealer', 'finished'))


@gen.coroutine
def run(loop):
    context = Context.instance()
    client = context.socket(zmq.ROUTER)
    client.bind("ipc://routing.ipc")
    responses = yield [
        worker_a(context),
        worker_b(context),
        dealer(client),
    ]
    print('responses: {}'.format(responses))


def main():
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    try:
        loop = IOLoop.current()
        loop.run_sync(partial(run, loop))
        print('(main) exiting')
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')
        sys.exit(0)


if __name__ == '__main__':
    main()
    print('(program) finished')
