#!/usr/bin/env python

"""
synopsis:
    Hello World client in Python.
    Implemented with ioloop and coroutines.
    Connects REQ socket to Url.
    Sends "Hello" to server; expects reply back.
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python hwclient.py <ident>
notes:
    <ident> is a string used to identify this client and to determine
        whether the right requests are returned to the correct client.
    Before starting this client, start either hwserver.py or mtserver.py.
"""


# from __future__ import print_function
import sys
import zmq
from zmq.eventloop.future import Context
from zmq.eventloop.ioloop import IOLoop
from tornado import gen


Url = 'tcp://127.0.0.1:5555'
Ctx = Context()


@gen.coroutine
def run(ident):
    #  Socket to talk to server
    print(("Client {} connecting to hello world server.  " +
          "Ctrl-C to exit early.").format(ident))
    socket = Ctx.socket(zmq.REQ)
    socket.connect(Url)
    #  Do multiple requests, waiting each time for a response
    for request in range(10):
        message = '{} Hello {}'.format(ident, request)
        message = message.encode('utf-8')
        print("Client {} sending message: {}".format(ident, message))
        yield socket.send(message)
        #  Get the reply.
        message = yield socket.recv()
        print("Client {} received reply: {}".format(ident, message))
    print('exiting')
    raise gen.Return('nothing')


def main():
    args = sys.argv[1:]
    if len(args) != 1:
        sys.exit(__doc__)
    ident = args[0]
    try:
        loop = IOLoop.current()
        loop.run_sync(lambda: run(ident))
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')


if __name__ == '__main__':
    main()
