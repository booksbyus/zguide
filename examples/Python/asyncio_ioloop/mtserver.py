#!/usr/bin/env python

"""
synopsis:
    Multitasking Hello World server.  Starts multiple worker callbacks
    to handle requests.
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python mtserver.py
"""


# from __future__ import print_function
import sys
from functools import partial
import zmq
from zmq.asyncio import Context, Poller, ZMQEventLoop
import asyncio


Ctx = Context()
Url_worker = "inproc://workers"
Url_client = "tcp://*:5555"


DEBUG = True


def printdbg(*args):
    if DEBUG:
        print(*args)


@asyncio.coroutine
def worker_routine(ident):
    """Worker routine"""
    # Socket to talk to dispatcher
    socket = Ctx.socket(zmq.REP)
    socket.connect(Url_worker)
    printdbg('(worker_routine) {} started'.format(ident))
    while True:
        printdbg('(worker_routine) waiting for request')
        message = yield from socket.recv()
        printdbg("(worker_routine) Received message parts: %s".format(
            message))
        printdbg("(worker_routine) Received message: %s".format(message))
        # Do some 'work'
        yield from asyncio.sleep(1)
        # Send reply back to client
        message = '{} world, from {}'.format(message, ident).encode('utf-8')
        yield from socket.send(message)
        printdbg('(worker_routine) sent message: {}'.format(message))


@asyncio.coroutine
def run(loop):
    """Server routine"""
    # Prepare our context and sockets
    # Socket to talk to clients
    clients = Ctx.socket(zmq.ROUTER)
    clients.bind(Url_client)
    workers = Ctx.socket(zmq.DEALER)
    workers.bind(Url_worker)
    # Start the workers
    # Caution: Do *not* use lambda to create the function call to the worker.
    #     lambda does not work correctly inside a for-statement.
    for idx in range(5):
        ident = 'worker {}'.format(idx)
        #loop.call_soon(partial(worker_routine, ident))
        yield from worker_routine(ident)
    print('(run) started worker routines')
    poller = Poller()
    poller.register(clients, zmq.POLLIN)
    poller.register(workers, zmq.POLLIN)
    print('mtserver ready for requests')
    while True:
        events = yield from poller.poll()
        events = dict(events)
        if clients in events:
            message = yield from clients.recv_multipart()
            printdbg('(run) received from client message_parts: {}'.format(
                message))
            client, empty, message = message[:3]
            printdbg('(run) received from client message: {}'.format(
                message))
            printdbg('(run) sending message to workers: {}'.format(message))
            yield from workers.send_multipart([client, b'', message])
        elif workers in events:
            message = yield from workers.recv_multipart()
            printdbg('(run) received from worker message_parts: {}'.format(
                message))
            client, empty, message = message[:3]
            printdbg('(run) received from worker message: {}'.format(
                message))
            yield from clients.send_multipart([client, b'', message])
            printdbg('(run) sent message to clients: {}'.format(message))
    loop.stop()


def main():
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    try:
        loop = ZMQEventLoop()
        asyncio.set_event_loop(loop)
        loop.run_until_complete(run(loop))
        loop.close()
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')


if __name__ == "__main__":
    main()
