#!/usr/bin/env python

"""
synopsis:
    Paranoid Pirate worker
    Original author: Daniel Lundin <dln(at)eintr(dot)org>
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python ppworker.py
notes:
    To test this, use the lazy pirate client.  To run this, start any number of
    ppworker.py processes, one instance of an ppqueue.py process, and any
    number lpclient.py processes, in any order.
"""

import sys
import time
from random import randint
import zmq
from zmq.asyncio import Context, Poller, ZMQEventLoop
import asyncio


HEARTBEAT_LIVENESS = 3
HEARTBEAT_INTERVAL = 1
INTERVAL_INIT = 1
INTERVAL_MAX = 32
#  Paranoid Pirate Protocol constants
PPP_READY = b"\x01"      # Signals worker is ready
PPP_HEARTBEAT = b"\x02"  # Signals worker heartbeat
BACK_END_ADDRESS = 'tcp://localhost:5556'


@asyncio.coroutine
def worker_socket(context, poller):
    """Helper function that returns a new configured socket
       connected to the Paranoid Pirate queue"""
    worker = context.socket(zmq.DEALER)    # DEALER
    identity = "%04X-%04X" % (randint(0, 0x10000), randint(0, 0x10000))
    identity = identity.encode('utf-8')
    worker.setsockopt(zmq.IDENTITY, identity)
    poller.register(worker, zmq.POLLIN)
    worker.connect(BACK_END_ADDRESS)
    yield from worker.send(PPP_READY)
    return worker


@asyncio.coroutine
def run_worker(context):
    poller = Poller()

    liveness = HEARTBEAT_LIVENESS
    interval = INTERVAL_INIT

    heartbeat_at = time.time() + HEARTBEAT_INTERVAL

    worker = yield from worker_socket(context, poller)
    cycles = 0
    while True:
        socks = yield from poller.poll(HEARTBEAT_INTERVAL * 1000)
        socks = dict(socks)

        # Handle worker activity on backend
        if socks.get(worker) == zmq.POLLIN:
            #  Get message
            #  - 3-part envelope + content -> request
            #  - 1-part HEARTBEAT -> heartbeat
            frames = yield from worker.recv_multipart()
            if not frames:
                break    # Interrupted

            if len(frames) == 3:
                # Simulate various problems, after a few cycles
                cycles += 1
                if cycles > 3 and randint(0, 5) == 0:
                    print("I: Simulating a crash")
                    break
                if cycles > 3 and randint(0, 5) == 0:
                    print("I: Simulating CPU overload")
                    yield from asyncio.sleep(3)
                print("I: Normal reply")
                yield from worker.send_multipart(frames)
                liveness = HEARTBEAT_LIVENESS
                yield from asyncio.sleep(1)  # Do some heavy work
            elif len(frames) == 1 and frames[0] == PPP_HEARTBEAT:
                print("I: Queue heartbeat")
                liveness = HEARTBEAT_LIVENESS
            else:
                print("E: Invalid message: %s" % frames)
            interval = INTERVAL_INIT
        else:
            liveness -= 1
            if liveness == 0:
                print("W: Heartbeat failure, can't reach queue")
                print("W: Reconnecting in %0.2fs..." % interval)
                yield from asyncio.sleep(interval)

                if interval < INTERVAL_MAX:
                    interval *= 2
                poller.unregister(worker)
                worker.setsockopt(zmq.LINGER, 0)
                worker.close()
                worker = yield from worker_socket(context, poller)
                liveness = HEARTBEAT_LIVENESS
        if time.time() > heartbeat_at:
            heartbeat_at = time.time() + HEARTBEAT_INTERVAL
            print("I: Worker heartbeat")
            yield from worker.send(PPP_HEARTBEAT)


@asyncio.coroutine
def run(loop):
    context = Context(1)
    while True:
        yield from run_worker(context)


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
