#!/usr/bin/env python

"""
synopsis:
    File Transfer model #3

    In which the client requests each chunk individually, using
    command pipelining to give us a credit-based flow control.
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python fileio3.py
notes:
    In order to run this program, you must create a data file named
    "testdata".  If it is large enough (bigger than CHUNK_SIZE), then
    it will be sent as multiple chunks.
"""

from __future__ import print_function
import sys
import os
import zmq
from zmq.asyncio import Context, ZMQEventLoop
import asyncio
from zhelpers import socket_set_hwm, zpipe

CHUNK_SIZE = 250000
PIPELINE = 10


@asyncio.coroutine
def client_task(ctx, pipe):
    dealer = ctx.socket(zmq.DEALER)
    socket_set_hwm(dealer, PIPELINE)
    dealer.connect("tcp://127.0.0.1:6000")
    credit = PIPELINE   # Up to PIPELINE chunks in transit
    total = 0           # Total bytes received
    chunks = 0          # Total chunks received
    offset = 0          # Offset of next chunk request
    while True:
        while credit:
            # ask for next chunk
            yield from dealer.send_multipart([
                b"fetch",
                b"%i" % total,
                b"%i" % CHUNK_SIZE,
            ])
            offset += CHUNK_SIZE
            credit -= 1
        try:
            chunk = yield from dealer.recv()
        except zmq.ZMQError as e:
            if e.errno == zmq.ETERM:
                return   # shutting down, quit
            else:
                raise
        chunks += 1
        credit += 1
        size = len(chunk)
        total += size
        if size < CHUNK_SIZE:
            break   # Last chunk received; exit
    yield from dealer.send_multipart([
        b"finish",
        b"-1",
        b"-1",
    ])
    message = "client received %i chunks, %i bytes" % (chunks, total)
    print(message)
    print('(client) finished')
    yield from pipe.send(b"OK")
    return ('client', message)


# The rest of the code is exactly the same as in model 2, except
# that we set the HWM on the server's ROUTER socket to PIPELINE
# to act as a sanity check.
@asyncio.coroutine
def server_task(ctx):
    file = open("testdata", "r")
    router = ctx.socket(zmq.ROUTER)
    socket_set_hwm(router, PIPELINE)
    router.bind("tcp://*:6000")
    count = 0
    total = 0
    while True:
        # First frame in each message is the sender identity
        # Second frame is "fetch" command
        try:
            msg = yield from router.recv_multipart()
        except zmq.ZMQError as e:
            if e.errno == zmq.ETERM:
                return   # shutting down, quit
            else:
                raise
        identity, command, offset_str, chunksz_str = msg
        if command == b"finish":
            break
        assert command == b"fetch"
        offset = int(offset_str)
        chunksz = int(chunksz_str)
        # Read chunk of data from file
        file.seek(offset, os.SEEK_SET)
        data = file.read(chunksz)
        data = data.encode('utf-8')
        # Send resulting chunk to client
        yield from router.send_multipart([identity, data])
        print('(server) sent chunk: {} size: {}'.format(count, len(data)))
        count += 1
        total += len(data)
    message = 'server sent {} chunks, {} bytes'.format(count, total)
    print('(server) finished')
    return ('server', message)


@asyncio.coroutine
def monitor(pipe):
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


def run(loop):
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
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')
        sys.exit(0)


if __name__ == '__main__':
    #import pdb; pdb.set_trace()
    main()
