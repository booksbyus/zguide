#!/usr/bin/env python

"""
synopsis:
    Weather update client.  Run clients in parallel.
    Connects SUB socket to tcp://localhost:5556
    Collects weather updates and finds avg temp in zipcode
    Runs multiple requestors in parallel.
    This version uses:
        - the async keyword instead of the coroutine decorator
        - the await keyword instead of yield
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python wuclient.py zipcode1 zipcode2 ...
notes:
    zipcoden should be  in the range 10000 - 10009.
"""

import sys
import zmq
from zmq.asyncio import Context, ZMQEventLoop
import asyncio

SERVER_ADDRESS = "tcp://localhost:5556"


async def run_client(context, zipcode):
    #  Socket to talk to server
    socket = context.socket(zmq.SUB)
    socket.connect(SERVER_ADDRESS)
    print('Collecting updates from weather server for zipcode: {}'.format(
        zipcode))
    socket.setsockopt_string(zmq.SUBSCRIBE, zipcode)
    # Process 5 updates
    total_temp = 0
    for update_nbr in range(5):
        string = await socket.recv()
        string = string.decode('utf-8')
        print('I: received -- string: "{}"'.format(string))
        zipcode, temperature, relhumidity = string.split()
        total_temp += int(temperature)
    result = "Average temperature for zipcode '%s' was %dF" % (
        zipcode, total_temp / update_nbr)
    return result


async def run_client_parallel(context, zipcode):
    result = await run_client(context, zipcode)
    return result


def run(loop, zipcodes):
    context = Context()
    tasks = [
        asyncio.ensure_future(run_client_parallel(context, zipcode))
        for zipcode in zipcodes
    ]
    loop.run_until_complete(asyncio.wait(tasks))
    for task in tasks:
        print('result:', task.result())


def main():
    args = sys.argv[1:]
    if len(args) < 1:
        sys.exit(__doc__)
    zipcodes = args
    print('Running async/await version.')
    try:
        loop = ZMQEventLoop()
        asyncio.set_event_loop(loop)
        run(loop, zipcodes)
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')


if __name__ == '__main__':
    #import pdb; pdb.set_trace()
    main()
