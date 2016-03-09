#!/usr/bin/env python

"""
synopsis:
    Custom routing Router to Mama (ROUTER to REQ)
    Author: Jeremy Avnet (brainsik) <spork(dash)zmq(at)theory(dot)org>
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python rtreq.py
"""

import sys
import random
import zmq
import zhelpers
from zmq.asyncio import Context, ZMQEventLoop
import asyncio

NBR_WORKERS = 10


@asyncio.coroutine
def worker_task(id, context=None):
    context = context or Context.instance()
    worker = context.socket(zmq.REQ)
    # We use a string identity for ease here
    zhelpers.set_id(worker)
    worker.connect("tcp://localhost:5671")
    total = 0
    while True:
        # Tell the router we're ready for work
        yield from worker.send(b"ready")
        # Get workload from router, until finished
        workload = yield from worker.recv()
        #print('worker {} received: {}'.format(id, workload))
        finished = workload == b"END"
        if finished:
            print("worker %d processed: %d tasks" % (id, total))
            break
        total += 1
        # Do some random work
        yield from asyncio.sleep(0.1 * random.random())
    return ('worker {}'.format(id), total)


@asyncio.coroutine
def requestor(client):
    for _ in range(NBR_WORKERS * 10):
        # LRU worker is next waiting in the queue
        address, empty, ready = yield from client.recv_multipart()
        yield from client.send_multipart([
            address,
            b'',
            b'This is the workload',
        ])
    # Now ask mama to shut down and report their results
    for _ in range(NBR_WORKERS):
        address, empty, ready = yield from client.recv_multipart()
        yield from client.send_multipart([
            address,
            b'',
            b'END',
        ])
    return ('requestor', 'finished')


def run(loop):
    context = Context.instance()
    client = context.socket(zmq.ROUTER)
    client.bind("tcp://*:5671")
    tasks = [
        asyncio.ensure_future(worker_task(idx)) for idx in range(NBR_WORKERS)
    ]
    tasks.append(asyncio.ensure_future(requestor(client)))
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
