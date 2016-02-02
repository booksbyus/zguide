#!/usr/bin/env python

"""
synopsis:
    Lazy Pirate client
    Use zmq_poll to do a safe request-reply
    To run, start lpserver and then randomly kill/restart it
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python lpclient.py
"""

import sys
import zmq
from zmq.asyncio import Context, Poller, ZMQEventLoop
import asyncio


REQUEST_TIMEOUT = 2500
REQUEST_RETRIES = 3
SERVER_ENDPOINT = "tcp://localhost:5555"


@asyncio.coroutine
def run_client(context):
    print("I: Connecting to server...")
    client = context.socket(zmq.REQ)
    client.connect(SERVER_ENDPOINT)
    poll = Poller()
    poll.register(client, zmq.POLLIN)
    sequence = 0
    retries_left = REQUEST_RETRIES
    while retries_left:
        sequence += 1
        request = str(sequence)
        print("I: Sending (%s)" % request)
        yield from client.send_string(request)
        expect_reply = True
        while expect_reply:
            socks = yield from poll.poll(REQUEST_TIMEOUT)
            socks = dict(socks)
            if socks.get(client) == zmq.POLLIN:
                reply = yield from client.recv()
                if not reply:
                    break
                if int(reply) == sequence:
                    print("I: Server replied OK (%s)" % reply)
                    retries_left = REQUEST_RETRIES
                    expect_reply = False
                else:
                    print("E: Malformed reply from server: %s" % reply)
            else:
                print("W: No response from server, retrying...")
                # Socket is confused. Close and remove it.
                print('W: confused')
                client.setsockopt(zmq.LINGER, 0)
                client.unbind(SERVER_ENDPOINT)
                #client.close()
                poll.unregister(client)
                retries_left -= 1
                if retries_left == 0:
                    print("E: Server seems to be offline, abandoning")
                    return
                print("I: Reconnecting and resending (%s)" % request)
                # Create new connection
                client = context.socket(zmq.REQ)
                client.connect(SERVER_ENDPOINT)
                poll.register(client, zmq.POLLIN)
                yield from client.send_string(request)


@asyncio.coroutine
def run(loop):
    context = Context()
    while True:
        yield from run_client(context)


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


if __name__ == '__main__':
    main()
