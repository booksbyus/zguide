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
from functools import partial
import zmq
from zmq.eventloop.future import Context
from zmq.eventloop.ioloop import IOLoop
from tornado import gen


@gen.coroutine
def run_sink(context):
    # Socket to receive messages on
    receiver = context.socket(zmq.PULL)
    receiver.bind("tcp://*:5558")
    # Socket for worker control
    controller = context.socket(zmq.PUB)
    controller.bind("tcp://*:5559")
    # Wait for start of batch
    yield receiver.recv()
    # Start our clock now
    tstart = time.time()
    # Process 100 confirmiations
    for task_nbr in range(100):
        yield receiver.recv()
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
    yield controller.send(b"KILL")
    # Finished
    #receiver.close()
    #controller.close()
    #context.term()


@gen.coroutine
def run(loop):
    context = Context()
    yield run_sink(context)


def main():
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    try:
        loop = IOLoop.current()
        loop.run_sync(partial(run, loop, ))
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')


if __name__ == '__main__':
    main()
