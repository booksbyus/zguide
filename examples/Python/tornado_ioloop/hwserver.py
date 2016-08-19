#!/usr/bin/env python

"""
synopsis:
    Hello World server in Python.
    Binds REP socket to Url.
    Expects b"Hello" from client, replies with b"World".
    Modified for tornado/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python hwserver.py
"""

import sys
import zmq
from zmq.eventloop.future import Context
from zmq.eventloop.ioloop import IOLoop
from tornado import gen


Url = 'tcp://127.0.0.1:5555'
Ctx = Context()


@gen.coroutine
def run():
    print("Getting ready for hello world client.  Ctrl-C to exit.\n")
    socket = Ctx.socket(zmq.REP)
    socket.bind(Url)
    while True:
        #  Wait for next request from client
        message = yield socket.recv()
        print("Received request: {}".format(message))
        #  Do some 'work'
        yield gen.sleep(1)
        #  Send reply back to client
        message = message.decode('utf-8')
        message = '{}, world'.format(message)
        message = message.encode('utf-8')
        print("Sending reply: {}".format(message))
        yield socket.send(message)


def main():
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    try:
        loop = IOLoop.current()
        loop.add_callback(lambda: run())
        loop.start()
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')
        sys.exit(0)


if __name__ == '__main__':
    main()
