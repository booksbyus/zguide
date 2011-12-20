"""
Suicidal Snail

Author: Min RK <benjaminrk@gmail.com>
"""

import sys
import threading
import time
import random

import zmq

from zhelpers import zpipe

# ---------------------------------------------------------------------
# This is our subscriber
# It connects to the publisher and subscribes to everything. It
# sleeps for a short time between messages to simulate doing too
# much work. If a message is more than 1 second late, it croaks.

MAX_ALLOWED_DELAY = 1.0    # secs

def subscriber(pipe):
    # Subscribe to everything
    ctx = zmq.Context.instance()
    sub = ctx.socket(zmq.SUB)
    sub.setsockopt(zmq.SUBSCRIBE, '')
    sub.connect("tcp://localhost:5556")

    # Get and process messages
    while True:
        clock = float(sub.recv())
        # Suicide snail logic
        if (time.time() - clock > MAX_ALLOWED_DELAY):
            print >> sys.stderr, "E: subscriber cannot keep up, aborting\n",
            break
        
        # Work for 1 msec plus some random additional time
        time.sleep(1e-3 * (1+2*random.random()))
    pipe.send("gone and died")


# ---------------------------------------------------------------------
# This is our server task
# It publishes a time-stamped message to its pub socket every 1ms.

def publisher(pipe):
    # Prepare publisher
    ctx = zmq.Context.instance()
    pub = ctx.socket(zmq.PUB)
    pub.bind("tcp://*:5556")

    while True:
        # Send current clock (secs) to subscribers
        pub.send(str(time.time()))
        try:
            signal = pipe.recv(zmq.NOBLOCK)
        except zmq.ZMQError as e:
            if e.errno == zmq.EAGAIN:
                # nothing to recv
                pass
            else:
                raise
        else:
            # received break message
            break
        time.sleep(1e-3)            # 1msec wait


# This main thread simply starts a client, and a server, and then
# waits for the client to signal it's died.

def main():
    ctx = zmq.Context.instance()
    pub_pipe, pub_peer = zpipe(ctx)
    sub_pipe, sub_peer = zpipe(ctx)
    
    pub_thread = threading.Thread(target=publisher, args=(pub_peer,))
    pub_thread.daemon=True
    pub_thread.start()
    sub_thread = threading.Thread(target=subscriber, args=(sub_peer,))
    sub_thread.daemon=True
    sub_thread.start()
    # wait for sub to finish
    sub_pipe.recv()
    # tell pub to halt
    pub_pipe.send("break")
    time.sleep(0.1)

if __name__ == '__main__':
    main()