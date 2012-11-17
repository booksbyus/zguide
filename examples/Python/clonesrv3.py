"""
Clone server Model Three

Author: Min RK <benjaminrk@gmail.com
"""

import zmq

from kvsimple import KVMsg

# simple struct for routing information for a key-value snapshot
class Route:
    def __init__(self, socket, identity):
        self.socket = socket # ROUTER socket to send to
        self.identity = identity # Identity of peer who requested state

def send_single(key, kvmsg, route):
    """Send one state snapshot key-value pair to a socket"""
    # Send identity of recipient first
    route.socket.send(route.identity, zmq.SNDMORE)
    kvmsg.send(route.socket)

def main():
    # context and sockets
    ctx = zmq.Context()
    snapshot = ctx.socket(zmq.ROUTER)
    snapshot.bind("tcp://*:5556")
    publisher = ctx.socket(zmq.PUB)
    publisher.bind("tcp://*:5557")
    collector = ctx.socket(zmq.PULL)
    collector.bind("tcp://*:5558")

    sequence = 0
    kvmap = {}

    poller = zmq.Poller()
    poller.register(collector, zmq.POLLIN)
    poller.register(snapshot, zmq.POLLIN)
    while True:
        try:
            items = dict(poller.poll(1000))
        except:
            break           # Interrupted

        # Apply state update sent from client
        if collector in items:
            kvmsg = KVMsg.recv(collector)
            sequence += 1
            kvmsg.sequence = sequence
            kvmsg.send(publisher)
            kvmsg.store(kvmap)
            print "I: publishing update %5d" % sequence

        # Execute state snapshot request
        if snapshot in items:
            msg = snapshot.recv_multipart()
            identity = msg[0]
            request = msg[1]
            if request == "ICANHAZ?":
                pass
            else:
                print "E: bad request, aborting\n",
                break

            # Send state snapshot to client
            route = Route(snapshot, identity)

            # For each entry in kvmap, send kvmsg to client
            for k,v in kvmap.items():
                send_single(k,v,route)

            # Now send END message with sequence number
            print "Sending state shapshot=%d\n" % sequence,
            snapshot.send(identity, zmq.SNDMORE)
            kvmsg = KVMsg(sequence)
            kvmsg.key = "KTHXBAI"
            kvmsg.body = ""
            kvmsg.send(snapshot)

    print " Interrupted\n%d messages handled" % sequence


if __name__ == '__main__':
    main()
