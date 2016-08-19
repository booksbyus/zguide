#!/usr/bin/env python

"""
synopsis:
    Request-reply service in Python
    Connects REP socket to tcp://localhost:5560
    Expects "Hello" from client, replies with "World"
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python rrworker.py <ident> <number_of_workers>
        where:
            ident is a string to identify the set of workers
                created by running this script.
            number_of_workers is the number of workers to be created.
notes:
    To run this, start rrbroker.py, any number of instances of rrworker.py,
    and rrclient.py.
"""

import sys
import zmq
from functools import partial
from zmq.eventloop.future import Context
from zmq.eventloop.ioloop import IOLoop
from tornado import gen


@gen.coroutine
def run_worker(context, worker_id):
    socket = context.socket(zmq.REP)
    socket.connect("tcp://localhost:5560")
    while True:
        message = yield socket.recv()
        print("Worker {} received request: {}".format(worker_id, message))
        message = message.decode('utf-8')
        reply = '{}, World from worker {}'.format(message, worker_id)
        reply = reply.encode('utf-8')
        yield socket.send(reply)
        print("Worker {} sent reply: {}".format(worker_id, reply))


@gen.coroutine
def run_worker_parallel(context, ident, num_workers):
    yield [
        run_worker(context, '%s-%d' % (ident, idx))
        for idx in range(num_workers)
    ]


@gen.coroutine
def run(loop, ident, num_workers):
    context = Context()
    yield run_worker_parallel(context, ident, num_workers)


def main():
    args = sys.argv[1:]
    if len(args) != 2:
        sys.exit(__doc__)
    try:
        ident = args[0]
        num_workers = int(args[1])
        loop = IOLoop.current()
        loop.run_sync(partial(run, loop, ident, num_workers, ))
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')


if __name__ == '__main__':
    main()
