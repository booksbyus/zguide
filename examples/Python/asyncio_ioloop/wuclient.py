#!/usr/bin/env python

"""
synopsis:
    Weather update client
    Connects SUB socket to tcp://localhost:5556
    Collects weather updates and finds avg temp in zipcode
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python wuclient.py [zip_code]
notes:
    Include an optional zip code in the range 10000 - 10004
"""

import sys
import zmq
from zmq.asyncio import Context, ZMQEventLoop
import asyncio


SERVER_ADDRESS = "tcp://localhost:5556"


async def run_client(context, zip_filter):
    #  Socket to talk to server
    socket = context.socket(zmq.SUB)
    socket.connect(SERVER_ADDRESS)
    print("Collecting updates from weather server...")
    # Subscribe to zipcode, default is NYC, 10001
    # Python 2 - ascii bytes to unicode str
    if isinstance(zip_filter, bytes):
        zip_filter = zip_filter.decode('ascii')
    socket.setsockopt_string(zmq.SUBSCRIBE, zip_filter)
    # Process 5 updates
    total_temp = 0
    for update_nbr in range(5):
        string = await socket.recv()
        string = string.decode('utf-8')
        print('I: received -- string: "{}"'.format(string))
        zipcode, temperature, relhumidity = string.split()
        total_temp += int(temperature)
    print("Average temperature for zipcode '%s' was %dF" % (
          zip_filter, total_temp / update_nbr))


@asyncio.coroutine
def run(loop, zip_filter):
    context = Context()
    yield from run_client(context, zip_filter)


def main():
    args = sys.argv[1:]
    if len(args) > 1:
        sys.exit(__doc__)
    zip_filter = sys.argv[1] if len(sys.argv) > 1 else "10001"
    try:
        loop = ZMQEventLoop()
        asyncio.set_event_loop(loop)
        loop.run_until_complete(run(loop, zip_filter))
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')


if __name__ == '__main__':
    main()
