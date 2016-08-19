#!/usr/bin/env python

"""
synopsis:
    Pathological publisher
    Sends out 1,000 topics and then one random update per second
usage:
    python pathopub.py
"""

import sys
from random import randint
import zmq
from zmq.eventloop.future import Context
from zmq.eventloop.ioloop import IOLoop
from tornado import gen


Url = 'tcp://127.0.0.1:5555'
Ctx = Context()


@gen.coroutine
def run():
    publisher = Ctx.socket(zmq.PUB)
    publisher.bind(Url)
    # Ensure subscriber connection has time to complete
    yield gen.sleep(1)
    # Send out all 1,000 topic messages
    for topic_nbr in range(1000):
        yield publisher.send_multipart([
            b"%03d" % topic_nbr,
            b"Save Roger",
        ])
        print('.', end='')
    print()
    count = 0
    while True:
        count += 1
        # Send one random update per second
        yield gen.sleep(1)
        message = [
            b"%03d" % randint(0, 9),
            "Off with his head. {}".format(count).encode('utf-8'),
        ]
        yield publisher.send_multipart(message)
        print('sent message: {}'.format(message))


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


if __name__ == '__main__':
    main()
