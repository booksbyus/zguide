#!/usr/bin/env python

"""
synopsis:
    Multitasking Hello World server.  Starts multiple worker callbacks
    to handle requests.
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python mtserver.py
notes:
    To test this, start this process, then start one or more instances
    of the hwclient.py process.
"""


import sys
import asyncio
import zmq
from zmq.asyncio import Context, Poller, ZMQEventLoop


Ctx = Context()
Url_worker = "inproc://workers"
Url_client = "tcp://*:5555"


DEBUG = False


def printdbg(*args):
    if DEBUG:
        print(*args)


def hello_world(loop):
    print('Hello World')
    loop.stop()


def run_worker(ident):
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


def run_server(loop):
    """Server routine"""
    # Prepare our context and sockets
    # Socket to talk to clients
    clients = Ctx.socket(zmq.ROUTER)
    clients.bind(Url_client)
    workers = Ctx.socket(zmq.DEALER)
    workers.bind(Url_worker)
    # Start the workers
    tasks = []
    for idx in range(5):
        ident = 'worker {}'.format(idx)
        task = asyncio.ensure_future(run_worker(ident))
        tasks.append(task)
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


@asyncio.coroutine
def run(loop):
    yield from run_server(loop)


def main():
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    try:
        loop = ZMQEventLoop()
        asyncio.set_event_loop(loop)
        loop.run_until_complete(run(loop))
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')


if __name__ == "__main__":
    #import pdb; pdb.set_trace()
    main()
