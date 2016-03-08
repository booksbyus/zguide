#!/usr/bin/env python

"""
synopsis:
    Task sink - design 2
    Adds pub-sub flow to send kill signal to workers
    Author: Jeremy Avnet (brainsik) <spork(dash)zmq(at)theory(dot)org>
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python tasksink2.py
"""

import sys
import time
import zmq
from zmq.asyncio import Context, ZMQEventLoop
import asyncio


@asyncio.coroutine
def run_sink(context):
    # Socket to receive messages on
    receiver = context.socket(zmq.PULL)
    receiver.bind("tcp://*:5558")
    # Socket for worker control
    controller = context.socket(zmq.PUB)
    controller.bind("tcp://*:5559")
    # Wait for start of batch
    yield from receiver.recv()
    # Start our clock now
    tstart = time.time()
    # Process 100 confirmiations
    for task_nbr in range(100):
        yield from receiver.recv()
        if task_nbr % 10 == 0:
            sys.stdout.write(":")
        else:
            sys.stdout.write(".")
        sys.stdout.flush()
    # Calculate and report duration of batch
    tend = time.time()
    tdiff = tend - tstart
    total_msec = tdiff * 1000
    print("Total elapsed time: %d msec" % total_msec)
    # Send kill signal to workers
    yield from controller.send(b"KILL")
    # Finished
    #receiver.close()
    #controller.close()
    #context.term()


@asyncio.coroutine
def run(loop):
    context = Context()
    yield from run_sink(context)


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
