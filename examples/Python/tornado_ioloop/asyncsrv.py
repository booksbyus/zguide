#!/usr/bin/env python

"""
synopsis:
    Create in-process clients and workers.
    Implement a "proxy" to pass messages from clients to workers and back.
    Original author: "Felipe Cruz <felipecruz@loogica.net>"
    Modified for tornado/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python asyncsrv.py
"""

import sys
from functools import partial
import zmq
from zmq.eventloop.future import Context, Poller
from zmq.eventloop.ioloop import IOLoop
from tornado import gen

__author__ = "Felipe Cruz <felipecruz@loogica.net>"
__license__ = "MIT/X11"
#FRONTEND_ADDR = 'tcp://*:5570'
FRONTEND_ADDR = 'inproc://frontend'
BACKEND_ADDR = 'inproc://backend'


DEBUG = False


def printdbg(*args):
    if DEBUG:
        print(*args)


class Client(object):
    """A client that generates requests."""
    def __init__(self, context, id):
        self.context = context
        self.id = id

    @gen.coroutine
    def run_client(self):
        socket = self.context.socket(zmq.REQ)
        identity = u'client-%d' % self.id
        socket.connect(FRONTEND_ADDR)
        print('Client %s started' % (identity))
        reqs = 0
        while True:
            reqs = reqs + 1
            msg = 'request # {}.{}'.format(self.id, reqs)
            msg = msg.encode('utf-8')
            yield socket.send(msg)
            print('Client {} sent request: {}'.format(self.id, reqs))
            msg = yield socket.recv()
            print('Client %s received: %s' % (identity, msg))
            yield gen.sleep(1)
        #socket.close()
        #context.term()


class Server(object):
    """A server to set up and initialize clients and request handlers"""
    def __init__(self, loop, context):
        self.loop = loop
        self.context = context

    @gen.coroutine
    def run_server(self):
        printdbg('(Server.run) starting')
        frontend = self.context.socket(zmq.ROUTER)
        frontend.bind(FRONTEND_ADDR)
        backend = self.context.socket(zmq.DEALER)
        backend.bind(BACKEND_ADDR)
        self.loop.add_callback(partial(run_proxy, frontend, backend))
        printdbg('(Server.run) started proxy')
        yield gen.sleep(0.1)
        # Start up the workers.
        for idx in range(5):
            #ident = 'worker {}'.format(idx, self.context)
            worker = Worker(self.context, idx)
            self.loop.add_callback(worker.run_worker)
            printdbg('(Server.run) started worker {}'.format(idx))
        yield gen.sleep(0.1)
        # Start up the clients.
        clients = [Client(self.context, idx) for idx in range(3)]
        yield [client.run_client() for client in clients]
        printdbg('(Server.run) after starting clients')
        #frontend.close()
        #backend.close()
        #self.context.term()


class Worker(object):
    """A request handler"""
    def __init__(self, context, idx):
        self.context = context
        self.idx = idx

    @gen.coroutine
    def run_worker(self):
        worker = self.context.socket(zmq.DEALER)
        worker.connect(BACKEND_ADDR)
        print('Worker {} started'.format(self.idx))
        while True:
            ident, part2, msg = yield worker.recv_multipart()
            print('Worker %s received %s from %s' % (self.idx, msg, ident))
            yield gen.sleep(0.5)
            yield worker.send_multipart([ident, part2, msg])
        worker.close()


@gen.coroutine
def run_proxy(socket_from, socket_to):
    poller = Poller()
    poller.register(socket_from, zmq.POLLIN)
    poller.register(socket_to, zmq.POLLIN)
    printdbg('(run_proxy) started')
    while True:
        events = yield poller.poll()
        events = dict(events)
        if socket_from in events:
            msg = yield socket_from.recv_multipart()
            printdbg('(run_proxy) received from frontend -- msg: {}'.format(
                msg))
            yield socket_to.send_multipart(msg)
            printdbg('(run_proxy) sent to backend -- msg: {}'.format(msg))
        elif socket_to in events:
            msg = yield socket_to.recv_multipart()
            printdbg('(run_proxy) received from backend -- msg: {}'.format(
                msg))
            yield socket_from.send_multipart(msg)
            printdbg('(run_proxy) sent to frontend -- msg: {}'.format(msg))


@gen.coroutine
def run(loop):
    context = Context()
    server = Server(loop, context)
    yield server.run_server()
    printdbg('(run) finished')


def main():
    """main function"""
    print('(main) starting')
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    try:
        loop = IOLoop.current()
        loop.run_sync(partial(run, loop))
        print('(main) after starting run()')
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')


if __name__ == "__main__":
    #import pdb; pdb.set_trace()
    main()
