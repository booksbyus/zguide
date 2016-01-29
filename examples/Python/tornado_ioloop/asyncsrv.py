#!/usr/bin/env python

"""
synopsis:
"""

import sys
from functools import partial
from random import randint
import zmq
from zmq.eventloop.future import Context
from zmq.eventloop.ioloop import IOLoop
from tornado import gen


__author__ = "Felipe Cruz <felipecruz@loogica.net>"
__license__ = "MIT/X11"


DEBUG = True


def printdbg(*args):
    if DEBUG:
        tprint(*args)


def tprint(msg):
    """like print, but won't get newlines confused with multiple threads"""
    sys.stdout.write(msg + '\n')
    sys.stdout.flush()


class ClientTask(object):
    """ClientTask"""
    def __init__(self, id):
        self.id = id

    @gen.coroutine
    def run(self):
        context = Context()
        socket = context.socket(zmq.DEALER)
        identity = u'worker-%d' % self.id
        socket.identity = identity.encode('ascii')
        socket.connect('tcp://localhost:5570')
        print('Client %s started' % (identity))
        poll = zmq.Poller()
        poll.register(socket, zmq.POLLIN)
        reqs = 0
        while True:
            reqs = reqs + 1
            print('Req #%d sent..' % (reqs))
            yield socket.send_string(u'request #%d' % (reqs))
            for i in range(5):
                sockets = dict(poll.poll(1000))
                if socket in sockets:
                    msg = yield socket.recv()
                    tprint('Client %s received: %s' % (identity, msg))

        socket.close()
        context.term()


class ServerTask(object):
    """ServerTask"""
    def __init__(self):
        pass

    @gen.coroutine
    def run(self):
        context = Context()
        frontend = context.socket(zmq.ROUTER)
        frontend.bind('tcp://*:5570')

        backend = context.socket(zmq.DEALER)
        backend.bind('inproc://backend')

        workers = []
        for i in range(5):
            worker = ServerWorker(context)
            yield worker.run()
            workers.append(worker)

        zmq.proxy(frontend, backend)

        frontend.close()
        backend.close()
        context.term()


class ServerWorker(object):
    """ServerWorker"""
    def __init__(self, context):
        self.context = context

    @gen.coroutine
    def run(self):
        worker = self.context.socket(zmq.DEALER)
        worker.connect('inproc://backend')
        tprint('Worker started')
        while True:
            ident, msg = yield worker.recv_multipart()
            tprint('Worker received %s from %s' % (msg, ident))
            replies = randint(0, 4)
            for i in range(replies):
                #yield gen.sleep(1. / (randint(1, 10)))
                yield gen.sleep(1)
                yield worker.send_multipart([ident, msg])
                printdbg('worker sent msg: {}'.format(msg))

        worker.close()


@gen.coroutine
def run(loop):
    server = ServerTask()
    yield server.run()


def main():
    """main function"""
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    try:
        loop = IOLoop.current()
        for idx in range(3):
            loop.add_callback(partial(ClientTask, idx))
        loop.run_sync(partial(run, loop))
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')

##    server = ServerTask()
##    server.start()
##    for i in range(3):
##        client = ClientTask(i)
##        client.start()
##
##    server.join()


if __name__ == "__main__":
    main()
