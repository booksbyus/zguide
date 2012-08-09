"""
Clone client Model Three

Author: Min RK <benjaminrk@gmail.com
"""

import random
import time

import zmq

from kvsimple import KVMsg

def main():
    
    # Prepare our context and subscriber
    ctx = zmq.Context()
    snapshot = ctx.socket(zmq.DEALER)
    snapshot.linger = 0
    snapshot.connect("tcp://localhost:5556")
    subscriber = ctx.socket(zmq.SUB)
    subscriber.linger = 0
    subscriber.setsockopt(zmq.SUBSCRIBE, '')
    subscriber.connect("tcp://localhost:5557")
    publisher = ctx.socket(zmq.PUSH)
    publisher.linger = 0
    publisher.connect("tcp://localhost:5558")

    random.seed(time.time())
    kvmap = {}

    # Get state snapshot
    sequence = 0
    snapshot.send("ICANHAZ?")
    while True:
        try:
            kvmsg = KVMsg.recv(snapshot)
        except:
            return          # Interrupted
            
        if kvmsg.key == "KTHXBAI":
            sequence = kvmsg.sequence
            print "I: Received snapshot=%d" % sequence
            break          # Done
        kvmsg.store(kvmap)
    
    poller = zmq.Poller()
    poller.register(subscriber, zmq.POLLIN)
    
    alarm = time.time()+1.
    while True:
        tickless = 1000*max(0, alarm - time.time())
        try:
            items = dict(poller.poll(tickless))
        except:
            break           # Interrupted
        
        if subscriber in items:
            kvmsg = KVMsg.recv(subscriber)

            # Discard out-of-sequence kvmsgs, incl. heartbeats
            if kvmsg.sequence > sequence:
                sequence = kvmsg.sequence
                kvmsg.store(kvmap)
                print "I: received update=%d" % sequence
        
        # If we timed-out, generate a random kvmsg
        if time.time() >= alarm:
            kvmsg = KVMsg(0)
            kvmsg.key = "%d" % random.randint(1,10000)
            kvmsg.body = "%d" % random.randint(1,1000000)
            kvmsg.send(publisher)
            kvmsg.store(kvmap)
            alarm = time.time() + 1.

    print " Interrupted\n%d messages in" % sequence

if __name__ == '__main__':
    main()