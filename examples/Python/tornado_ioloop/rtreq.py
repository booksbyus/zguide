#!/usr/bin/env python

"""
synopsis:
    Custom routing Router to Mama (ROUTER to REQ)
    Author: Jeremy Avnet (brainsik) <spork(dash)zmq(at)theory(dot)org>
    Modified for tornado/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python rtreq.py
"""

import sys
import random
import zmq
from functools import partial
from zmq.eventloop.future import Context
from zmq.eventloop.ioloop import IOLoop
from tornado import gen
import zhelpers

NBR_WORKERS = 10


@gen.coroutine
def worker_task(id, context=None):
    context = context or Context.instance()
    worker = context.socket(zmq.REQ)
    # We use a string identity for ease here
    zhelpers.set_id(worker)
    worker.connect("tcp://localhost:5671")
    total = 0
    while True:
        # Tell the router we're ready for work
        yield worker.send(b"ready")
        # Get workload from router, until finished
        workload = yield worker.recv()
        #print('(worker {}) received: {}'.format(id, workload))
        finished = workload == b"END"
        if finished:
            print("worker %d processed: %d tasks" % (id, total))
            break
        total += 1
        # Do some random work
        yield gen.sleep(0.1 * random.random())
    raise gen.Return(('worker {}'.format(id), total))


@gen.coroutine
def requestor(client):
    for _ in range(NBR_WORKERS * 10):
        # LRU worker is next waiting in the queue
        address, empty, ready = yield client.recv_multipart()
        yield client.send_multipart([
            address,
            b'',
            b'This is the workload',
        ])
    # Now ask mama to shut down and report their results
    for _ in range(NBR_WORKERS):
        address, empty, ready = yield client.recv_multipart()
        yield client.send_multipart([
            address,
            b'',
            b'END',
        ])
    raise gen.Return(('requestor', 'finished'))


@gen.coroutine
def run(loop):
    context = Context.instance()
    client = context.socket(zmq.ROUTER)
    client.bind("tcp://*:5671")
    responses = yield [
        worker_task(idx) for idx in range(NBR_WORKERS)
    ] + [requestor(client)]
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
