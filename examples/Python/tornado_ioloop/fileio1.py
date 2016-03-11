#!/usr/bin/env python

"""
synopsis:
    File Transfer model #1

    In which the server sends the entire file to the client in
    large chunks with no attempt at flow control.
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python fileio1.py
notes:
    In order to run this program, you must create a data file named
    "testdata".  If it is large enough (bigger than CHUNK_SIZE), then
    it will be sent as multiple chunks.
"""

from __future__ import print_function
import sys
from functools import partial
import zmq
from zmq.eventloop.future import Context
from zmq.eventloop.ioloop import IOLoop
from tornado import gen
from zhelpers import socket_set_hwm, zpipe

CHUNK_SIZE = 250000


@gen.coroutine
def client_task(ctx, pipe):
    dealer = ctx.socket(zmq.DEALER)
    dealer.connect("tcp://127.0.0.1:6000")
    yield dealer.send(b"fetch")
    total = 0       # Total bytes received
    chunks = 0      # Total chunks received
    while True:
        try:
            chunk = yield dealer.recv()
        except zmq.ZMQError as e:
            if e.errno == zmq.ETERM:
                return   # shutting down, quit
            else:
                raise
        chunks += 1
        size = len(chunk)
        total += size
        if size == 0:
            break   # whole file received
    message = "%i chunks received, %i bytes" % (chunks, total)
    print(message)
    yield pipe.send(b"OK")
    print('(client_task) finished')
    raise gen.Return(('client', message))


# File server thread
# The server thread reads the file from disk in chunks, and sends
# each chunk to the client as a separate message. We only have one
# test file, so open that once and then serve it out as needed:
@gen.coroutine
def server_task(ctx):
    file = open("testdata", "r")
    router = ctx.socket(zmq.ROUTER)
    # Default HWM is 1000, which will drop messages here
    # since we send more than 1,000 chunks of test data,
    # so set an infinite HWM as a simple, stupid solution:
    socket_set_hwm(router, 0)
    router.bind("tcp://*:6000")
    count = 0
    while True:
        # First frame in each message is the sender identity
        # Second frame is "fetch" command
        try:
            identity, command = yield router.recv_multipart()
            break
        except zmq.ZMQError as e:
            if e.errno == zmq.ETERM:
                return   # shutting down, quit
            else:
                raise
    assert command == b"fetch"
    while True:
        data = file.read(CHUNK_SIZE)
        data = data.encode('utf-8')
        yield router.send_multipart([identity, data])
        if not data:
            break
        count += 1
    message = 'server sent {} chunks'.format(count)
    print(message)
    print('(server_task) finished')
    raise gen.Return(('server', message))


@gen.coroutine
def monitor(pipe):
    # loop until client tells us it's done
    message = None
    try:
        mesg = yield pipe.recv()
        message = 'monitor received: {}'.format(mesg)
        print(message)
    except KeyboardInterrupt:
        pass
    print('(monitor) finished')
    raise gen.Return(('monitor', message))


# File main thread
# The main task starts the client and server threads; it's easier
# to test this as a single process with threads, than as multiple
# processes:
@gen.coroutine
def run(loop):
    ctx = Context()
    a, b = zpipe(ctx)
    responses = yield [
        client_task(ctx, b),
        server_task(ctx),
        monitor(a),
    ]
    print('responses: {}'.format(responses))
    del a, b
    print('(run) finished')


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
