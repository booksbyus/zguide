#!/usr/bin/env python

"""
synopsis:
    File Transfer model #2

    In which the client requests each chunk individually, thus
    eliminating server queue overflows, but at a cost in speed.
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python fileio2.py
notes:
    In order to run this program, you must create a data file named
    "testdata".  If it is large enough (bigger than CHUNK_SIZE), then
    it will be sent as multiple chunks.
"""

from __future__ import print_function
import sys
import os
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
    socket_set_hwm(dealer, 1)
    dealer.connect("tcp://127.0.0.1:6000")
    total = 0       # Total bytes received
    chunks = 0      # Total chunks received
    while True:
        # ask for next chunk
        yield dealer.send_multipart([
            b"fetch",
            b"%i" % total,
            b"%i" % CHUNK_SIZE
        ])
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
        if size < CHUNK_SIZE:
            break   # Last chunk received; exit
    yield dealer.send_multipart([
        b"finish",
        b"-1",
        b"-1",
    ])
    message = "client received %i chunks, %i bytes" % (chunks, total)
    print(message)
    yield pipe.send(b"OK")
    print('(client) finished')
    raise gen.Return(('client', message))


# File server thread
# The server thread waits for a chunk request from a client,
# reads that chunk and sends it back to the client:
@gen.coroutine
def server_task(ctx):
    file = open("testdata", "r")
    router = ctx.socket(zmq.ROUTER)
    router.bind("tcp://*:6000")
    count = 0
    total = 0
    while True:
        # First frame in each message is the sender identity
        # Second frame is "fetch" command
        try:
            msg = yield router.recv_multipart()
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
        total += len(data)
        # Send resulting chunk to client
        yield router.send_multipart([identity, data])
        count += 1
    message = 'server sent {} chunks, {} bytes'.format(count, total)
    print('(server) finished')
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


# The main process.
# The main task starts the client and server tasks; it's easier
# to test this as a single process with multiple tasks, than as multiple
# processes.
# The main task is just the same as in the first model.
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
