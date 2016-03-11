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
from zmq.asyncio import Context, ZMQEventLoop
import asyncio


@asyncio.coroutine
def run_worker(context, worker_id):
    socket = context.socket(zmq.REP)
    socket.connect("tcp://localhost:5560")
    while True:
        message = yield from socket.recv()
        print("Worker {} received request: {}".format(worker_id, message))
        message = message.decode('utf-8')
        reply = '{}, World from worker {}'.format(message, worker_id)
        reply = reply.encode('utf-8')
        yield from socket.send(reply)
        print("Worker {} sent reply: {}".format(worker_id, reply))


def run(loop, ident, num_workers):
    context = Context()
    tasks = [
        asyncio.ensure_future(run_worker(context, '%s-%d' % (ident, idx)))
        for idx in range(num_workers)
    ]
    loop.run_until_complete(asyncio.wait(tasks))


def main():
    args = sys.argv[1:]
    if len(args) != 2:
        sys.exit(__doc__)
    try:
        ident = args[0]
        num_workers = int(args[1])
        loop = ZMQEventLoop()
        asyncio.set_event_loop(loop)
        run(loop, ident, num_workers)
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')


if __name__ == '__main__':
    main()
