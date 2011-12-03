"""
Clone Client Model One

Author: Min RK <benjaminrk@gmail.com>

"""

import random
import time

import zmq

from kvsimple import KVMsg

def main():
    # Prepare our context and publisher socket
    ctx = zmq.Context()
    updates = ctx.socket(zmq.SUB)
    updates.linger = 0
    updates.setsockopt(zmq.SUBSCRIBE, '')
    updates.connect("tcp://localhost:5556")

    kvmap = {}
    sequence = 0

    while True:
        try:
            kvmsg = KVMsg.recv(updates)
        except:
            break # Interrupted
        kvmsg.store(kvmap)
        sequence += 1
    print "Interrupted\n%d messages in" % sequence


if __name__ == '__main__':
    main()