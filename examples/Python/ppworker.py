#
##  Paranoid Pirate worker
#
#   Author: Daniel Lundin <dln(at)eintr(dot)org>
#

from random import randint
import time

import zmq

HEARTBEAT_LIVENESS = 3
HEARTBEAT_INTERVAL = 1
INTERVAL_INIT = 1
INTERVAL_MAX = 32

#  Paranoid Pirate Protocol constants
PPP_READY = "\x01"      # Signals worker is ready
PPP_HEARTBEAT = "\x02"  # Signals worker heartbeat

def worker_socket(context, poller):
    """Helper function that returns a new configured socket
       connected to the Paranoid Pirate queue"""
    worker = context.socket(zmq.DEALER) # DEALER
    identity = "%04X-%04X" % (randint(0, 0x10000), randint(0, 0x10000))
    worker.setsockopt(zmq.IDENTITY, identity)
    poller.register(worker, zmq.POLLIN)
    worker.connect("tcp://localhost:5556")
    worker.send(PPP_READY)
    return worker


context = zmq.Context(1)
poller = zmq.Poller()

liveness = HEARTBEAT_LIVENESS
interval = INTERVAL_INIT

heartbeat_at = time.time() + HEARTBEAT_INTERVAL

worker = worker_socket(context, poller)
cycles = 0
while True:
    socks = dict(poller.poll(HEARTBEAT_INTERVAL * 1000))

    # Handle worker activity on backend
    if socks.get(worker) == zmq.POLLIN:
        #  Get message
        #  - 3-part envelope + content -> request
        #  - 1-part HEARTBEAT -> heartbeat
        frames = worker.recv_multipart()
        if not frames:
            break # Interrupted

        if len(frames) == 3:
            # Simulate various problems, after a few cycles
            cycles += 1
            if cycles > 3 and randint(0, 5) == 0:
                print "I: Simulating a crash"
                break
            if cycles > 3 and randint(0, 5) == 0:
                print "I: Simulating CPU overload"
                time.sleep(3)
            print "I: Normal reply"
            worker.send_multipart(frames)
            liveness = HEARTBEAT_LIVENESS
            time.sleep(1)  # Do some heavy work
        elif len(frames) == 1 and frames[0] == PPP_HEARTBEAT:
            print "I: Queue heartbeat"
            liveness = HEARTBEAT_LIVENESS
        else:
            print "E: Invalid message: %s" % frames
        interval = INTERVAL_INIT
    else:
        liveness -= 1
        if liveness == 0:
            print "W: Heartbeat failure, can't reach queue"
            print "W: Reconnecting in %0.2fs..." % interval
            time.sleep(interval)

            if interval < INTERVAL_MAX:
                interval *= 2
            poller.unregister(worker)
            worker.setsockopt(zmq.LINGER, 0)
            worker.close()
            worker = worker_socket(context, poller)
            liveness = HEARTBEAT_LIVENESS
    if time.time() > heartbeat_at:
        heartbeat_at = time.time() + HEARTBEAT_INTERVAL
        print "I: Worker heartbeat"
        worker.send(PPP_HEARTBEAT)


