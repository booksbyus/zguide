#!/usr/bin/env python

"""
synopsis:
    Pathological subscriber
    Subscribes to one random topic and prints received messages
usage:
    python pathosub.py
"""

import sys
import zmq
from zmq.asyncio import Context, Poller, ZMQEventLoop
import asyncio


Url = 'tcp://127.0.0.1:5555'
Ctx = Context()


@asyncio.coroutine
def run():
    subscriber = Ctx.socket(zmq.SUB)
    subscriber.connect(Url)
    subscription = b"%03d" % 5
    subscriber.setsockopt(zmq.SUBSCRIBE, subscription)
    poller = Poller()
    poller.register(subscriber, zmq.POLLOUT)
    while True:
        topic, data = yield from subscriber.recv_multipart()
        #assert topic == subscription
        print(data)


def main():
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    try:
        loop = ZMQEventLoop()
        asyncio.set_event_loop(loop)
        loop.run_until_complete(run())
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')


if __name__ == '__main__':
    main()
