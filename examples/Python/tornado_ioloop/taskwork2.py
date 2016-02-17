#!/usr/bin/env python

"""
synopsis:
    Task worker - design 2
    Adds pub-sub flow to receive and respond to kill signal
    Author: Jeremy Avnet (brainsik) <spork(dash)zmq(at)theory(dot)org>
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python taskwork2.py
"""

import sys
from functools import partial
import zmq
from zmq.eventloop.future import Context, Poller
from zmq.eventloop.ioloop import IOLoop
from tornado import gen


@gen.coroutine
def run_worker(context):
    # Socket to receive messages on
    receiver = context.socket(zmq.PULL)
    receiver.connect("tcp://localhost:5557")
    # Socket to send messages to
    sender = context.socket(zmq.PUSH)
    sender.connect("tcp://localhost:5558")
    # Socket for control input
    controller = context.socket(zmq.SUB)
    controller.connect("tcp://localhost:5559")
    controller.setsockopt(zmq.SUBSCRIBE, b"")
    # Process messages from receiver and controller
    poller = Poller()
    poller.register(receiver, zmq.POLLIN)
    poller.register(controller, zmq.POLLIN)
    # Process messages from both sockets
    while True:
        socks = yield poller.poll()
        socks = dict(socks)
        if socks.get(receiver) == zmq.POLLIN:
            message = yield receiver.recv()
            # Process task
            workload = int(message)  # Workload in msecs
            # Do the work
            yield gen.sleep(workload / 1000.0)
            # Send results to sink
            yield sender.send(message)
            # Simple progress indicator for the viewer
            sys.stdout.write(".")
            sys.stdout.flush()
        # Any waiting controller command acts as 'KILL'
        if socks.get(controller) == zmq.POLLIN:
            break
    # Finished
    #receiver.close()
    #sender.close()
    #controller.close()
    #context.term()


@gen.coroutine
def run(loop):
    context = Context()
    yield run_worker(context)


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
