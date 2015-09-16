"""
Clone server Model Five

Author: Min RK <benjaminrk@gmail.com
"""

import logging
import time

import zmq
from zmq.eventloop.ioloop import IOLoop, PeriodicCallback
from zmq.eventloop.zmqstream import ZMQStream

from kvmsg import KVMsg
from zhelpers import dump

# simple struct for routing information for a key-value snapshot
class Route:
    def __init__(self, socket, identity, subtree):
        self.socket = socket        # ROUTER socket to send to
        self.identity = identity    # Identity of peer who requested state
        self.subtree = subtree      # Client subtree specification


def send_single(key, kvmsg, route):
    """Send one state snapshot key-value pair to a socket"""
    # check front of key against subscription subtree:
    if kvmsg.key.startswith(route.subtree):
        # Send identity of recipient first
        route.socket.send(route.identity, zmq.SNDMORE)
        kvmsg.send(route.socket)

class CloneServer(object):

    # Our server is defined by these properties
    ctx = None                  # Context wrapper
    kvmap = None                # Key-value store
    loop = None                 # IOLoop reactor
    port = None                 # Main port we're working on
    sequence = 0                # How many updates we're at
    snapshot = None             # Handle snapshot requests
    publisher = None            # Publish updates to clients
    collector = None            # Collect updates from clients

    def __init__(self, port=5556):
        self.port = port
        self.ctx = zmq.Context()
        self.kvmap = {}
        self.loop = IOLoop.instance()

        # Set up our clone server sockets
        self.snapshot  = self.ctx.socket(zmq.ROUTER)
        self.publisher = self.ctx.socket(zmq.PUB)
        self.collector = self.ctx.socket(zmq.PULL)
        self.snapshot.bind("tcp://*:%d" % self.port)
        self.publisher.bind("tcp://*:%d" % (self.port + 1))
        self.collector.bind("tcp://*:%d" % (self.port + 2))

        # Wrap sockets in ZMQStreams for IOLoop handlers
        self.snapshot = ZMQStream(self.snapshot)
        self.publisher = ZMQStream(self.publisher)
        self.collector = ZMQStream(self.collector)

        # Register our handlers with reactor
        self.snapshot.on_recv(self.handle_snapshot)
        self.collector.on_recv(self.handle_collect)
        self.flush_callback = PeriodicCallback(self.flush_ttl, 1000)

        # basic log formatting:
        logging.basicConfig(format="%(asctime)s %(message)s", datefmt="%Y-%m-%d %H:%M:%S",
                level=logging.INFO)


    def start(self):
        # Run reactor until process interrupted
        self.flush_callback.start()
        try:
            self.loop.start()
        except KeyboardInterrupt:
            pass

    def handle_snapshot(self, msg):
        """snapshot requests"""
        if len(msg) != 3 or msg[1] != "ICANHAZ?":
            print "E: bad request, aborting"
            dump(msg)
            self.loop.stop()
            return
        identity, request, subtree = msg
        if subtree:
            # Send state snapshot to client
            route = Route(self.snapshot, identity, subtree)

            # For each entry in kvmap, send kvmsg to client
            for k,v in self.kvmap.items():
                send_single(k,v,route)

            # Now send END message with sequence number
            logging.info("I: Sending state shapshot=%d" % self.sequence)
            self.snapshot.send(identity, zmq.SNDMORE)
            kvmsg = KVMsg(self.sequence)
            kvmsg.key = "KTHXBAI"
            kvmsg.body = subtree
            kvmsg.send(self.snapshot)

    def handle_collect(self, msg):
        """Collect updates from clients"""
        kvmsg = KVMsg.from_msg(msg)
        self.sequence += 1
        kvmsg.sequence = self.sequence
        kvmsg.send(self.publisher)
        ttl = float(kvmsg.get('ttl', 0))
        if ttl:
            kvmsg['ttl'] = time.time() + ttl
        kvmsg.store(self.kvmap)
        logging.info("I: publishing update=%d", self.sequence)

    def flush_ttl(self):
        """Purge ephemeral values that have expired"""
        for key,kvmsg in self.kvmap.items():
            self.flush_single(kvmsg)

    def flush_single(self, kvmsg):
        """If key-value pair has expired, delete it and publish the fact
        to listening clients."""
        ttl = float(kvmsg.get('ttl', 0))
        if ttl and ttl <= time.time():
            kvmsg.body = ""
            self.sequence += 1
            kvmsg.sequence = self.sequence
            kvmsg.send(self.publisher)
            del self.kvmap[kvmsg.key]
            logging.info("I: publishing delete=%d", self.sequence)

def main():
    clone = CloneServer()
    clone.start()

if __name__ == '__main__':
    main()
