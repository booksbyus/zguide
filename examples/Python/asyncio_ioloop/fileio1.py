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
import zmq
from zmq.asyncio import Context, ZMQEventLoop
import asyncio
from zhelpers import socket_set_hwm, zpipe

CHUNK_SIZE = 250000


@asyncio.coroutine
def client_task(ctx, pipe):
    print('(client) starting')
    dealer = ctx.socket(zmq.DEALER)
    dealer.connect("tcp://127.0.0.1:6000")
    yield from dealer.send(b"fetch")
    print('(client) sent fetch request')
    total = 0       # Total bytes received
    chunks = 0      # Total chunks received
    while True:
        try:
            chunk = yield from dealer.recv()
        except zmq.ZMQError as e:
            if e.errno == zmq.ETERM:
                return   # shutting down, quit
            else:
                raise
        chunks += 1
        size = len(chunk)
        total += size
        print('(client) received chunk: {}'.format(size))
        if size == 0:
            break   # whole file received
    message = "%i chunks received, %i bytes" % (chunks, total)
    print(message)
    yield from pipe.send(b"OK")
    print('(client_task) finished')
    return ('client', message)


# File server thread
# The server thread reads the file from disk in chunks, and sends
# each chunk to the client as a separate message. We only have one
# test file, so open that once and then serve it out as needed:
@asyncio.coroutine
def server_task(ctx):
    print('(server) starting')
    infile = open("testdata", "r")
    print('(server) setup 1')
    router = ctx.socket(zmq.ROUTER)
    # Default HWM is 1000, which will drop messages here
    # since we send more than 1,000 chunks of test data,
    # so set an infinite HWM as a simple, stupid solution:
    print('(server) setup 2')
    socket_set_hwm(router, 0)
    router.bind("tcp://*:6000")
    print('(server) after setup')
    count = 0
    total = 0
    while True:
        # First frame in each message is the sender identity
        # Second frame is "fetch" command
        print('(server) waiting for request')
        try:
            identity, command = yield from router.recv_multipart()
            break
        except zmq.ZMQError as e:
            if e.errno == zmq.ETERM:
                return   # shutting down, quit
            else:
                raise
    print('(server) received request.  command: {}'.format(command))
    assert command == b"fetch"
    while True:
        data = infile.read(CHUNK_SIZE)
        data = data.encode('utf-8')
        yield from router.send_multipart([identity, data])
        print('(server) sent chunk.  length: {}'.format(len(data)))
        if not data:
            break
        count += 1
        total += len(data)
    message = 'server sent {} chunks, {} bytes'.format(count, total)
    print(message)
    print('(server_task) finished')
    return ('server', message)


@asyncio.coroutine
def monitor(pipe):
    print('(monitor) starting')
    # loop until client tells us it's done
    message = None
    try:
        mesg = yield from pipe.recv()
        message = 'monitor received: {}'.format(mesg)
        print(message)
    except KeyboardInterrupt:
        pass
    print('(monitor) finished')
    return ('monitor', message)


# File main thread
# The main task starts the client and server threads; it's easier
# to test this as a single process with threads, than as multiple
# processes:
def run(loop):
    print('(run) starting')
    ctx = Context()
    a, b = zpipe(ctx)
    tasks = [
        asyncio.ensure_future(client_task(ctx, b)),
        asyncio.ensure_future(server_task(ctx)),
        asyncio.ensure_future(monitor(a)),
    ]
    loop.run_until_complete(asyncio.wait(tasks))
    results = [task.result() for task in tasks]
    print('results: {}'.format(results))
    del a, b
    print('(run) finished')


def main():
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    try:
        loop = ZMQEventLoop()
        asyncio.set_event_loop(loop)
        run(loop)
        print('(main) exiting')
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')
        sys.exit(0)


if __name__ == '__main__':
    #import pdb; pdb.set_trace()
    main()
