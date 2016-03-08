#!/usr/bin/env python

"""
synopsis:
    Request-reply client in Python
    Connects REQ socket to tcp://localhost:5559
    Sends "Hello" to server, expects "World" back
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python rrclient.py [num_requests]
        where:
            num_requests is the number of requests to be sent.  Default
                if omitted is 10.
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
def run_client(context, num_requests):
    #  Prepare our context and sockets
    socket = context.socket(zmq.REQ)
    socket.connect("tcp://localhost:5559")
    #  Do num_requests requests, waiting each time for a response
    for requestno in range(1, num_requests + 1):
        message = 'Hello {}'.format(requestno)
        message = message.encode('utf-8')
        yield socket.send(message)
        message = yield socket.recv()
        print("Received reply %s [%s]" % (requestno, message))


@gen.coroutine
def run(loop, num_requests):
    context = Context()
    yield run_client(context, num_requests)


def main():
    args = sys.argv[1:]
    if len(args) == 1:
        num_requests = int(args[0])
    elif len(args) == 0:
        num_requests = 10
    else:
        sys.exit(__doc__)
    try:
        loop = IOLoop.current()
        loop.run_sync(partial(run, loop, num_requests, ))
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')


if __name__ == '__main__':
    main()
