"""
Clone server Model Six

Author: Min RK <benjaminrk@gmail.com
"""

import logging
import time

import zmq
from zmq.eventloop.ioloop import PeriodicCallback
from zmq.eventloop.zmqstream import ZMQStream

from bstar import BinaryStar
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
    bstar = None                # Binary Star
    sequence = 0                # How many updates so far
    port = None                 # Main port we're working on
    peer = None                 # Main port of our peer
    publisher = None            # Publish updates and hugz
    collector = None            # Collect updates from clients
    subscriber = None           # Get updates from peer
    pending = None              # Pending updates from client
    primary = False             # True if we're primary
    master = False              # True if we're master
    slave = False               # True if we're slave

    def __init__(self, primary=True, ports=(5556,5566)):
        self.primary = primary
        if primary:
            self.port, self.peer = ports
            frontend = "tcp://*:5003"
            backend  = "tcp://localhost:5004"
            self.kvmap = {}
        else:
            self.peer, self.port = ports
            frontend = "tcp://*:5004"
            backend  = "tcp://localhost:5003"

        self.ctx = zmq.Context.instance()
        self.pending = []
        self.bstar = BinaryStar(primary, frontend, backend)

        self.bstar.register_voter("tcp://*:%i" % self.port, zmq.ROUTER, self.handle_snapshot)

        # Set up our clone server sockets
        self.publisher = self.ctx.socket(zmq.PUB)
        self.collector = self.ctx.socket(zmq.SUB)
        self.collector.setsockopt(zmq.SUBSCRIBE, b'')
        self.publisher.bind("tcp://*:%d" % (self.port + 1))
        self.collector.bind("tcp://*:%d" % (self.port + 2))

        # Set up our own clone client interface to peer
        self.subscriber = self.ctx.socket(zmq.SUB)
        self.subscriber.setsockopt(zmq.SUBSCRIBE, b'')
        self.subscriber.connect("tcp://localhost:%d" % (self.peer + 1))

        # Register state change handlers
        self.bstar.master_callback = self.become_master
        self.bstar.slave_callback = self.become_slave

        # Wrap sockets in ZMQStreams for IOLoop handlers
        self.publisher = ZMQStream(self.publisher)
        self.subscriber = ZMQStream(self.subscriber)
        self.collector = ZMQStream(self.collector)

        # Register our handlers with reactor
        self.collector.on_recv(self.handle_collect)
        self.flush_callback = PeriodicCallback(self.flush_ttl, 1000)
        self.hugz_callback = PeriodicCallback(self.send_hugz, 1000)

        # basic log formatting:
        logging.basicConfig(format="%(asctime)s %(message)s", datefmt="%Y-%m-%d %H:%M:%S",
                level=logging.INFO)

    def start(self):
        # start periodic callbacks
        self.flush_callback.start()
        self.hugz_callback.start()
        # Run bstar reactor until process interrupted
        try:
            self.bstar.start()
        except KeyboardInterrupt:
            pass

    def handle_snapshot(self, socket, msg):
        """snapshot requests"""
        if msg[1] != "ICANHAZ?" or len(msg) != 3:
            logging.error("E: bad request, aborting")
            dump(msg)
            self.bstar.loop.stop()
            return
        identity, request = msg[:2]
        if len(msg) >= 3:
            subtree = msg[2]
            # Send state snapshot to client
            route = Route(socket, identity, subtree)

            # For each entry in kvmap, send kvmsg to client
            for k,v in self.kvmap.items():
                send_single(k,v,route)

            # Now send END message with sequence number
            logging.info("I: Sending state shapshot=%d" % self.sequence)
            socket.send(identity, zmq.SNDMORE)
            kvmsg = KVMsg(self.sequence)
            kvmsg.key = "KTHXBAI"
            kvmsg.body = subtree
            kvmsg.send(socket)

    def handle_collect(self, msg):
        """Collect updates from clients

        If we're master, we apply these to the kvmap
        If we're slave, or unsure, we queue them on our pending list
        """
        kvmsg = KVMsg.from_msg(msg)
        if self.master:
            self.sequence += 1
            kvmsg.sequence = self.sequence
            kvmsg.send(self.publisher)
            ttl = float(kvmsg.get('ttl', 0))
            if ttl:
                kvmsg['ttl'] = time.time() + ttl
            kvmsg.store(self.kvmap)
            logging.info("I: publishing update=%d", self.sequence)
        else:
            # If we already got message from master, drop it, else
            # hold on pending list
            if not self.was_pending(kvmsg):
                self.pending.append(kvmsg)

    def was_pending(self, kvmsg):
        """If message was already on pending list, remove and return True.
        Else return False.
        """
        found = False
        for idx, held in enumerate(self.pending):
            if held.uuid == kvmsg.uuid:
                found = True
                break
        if found:
            self.pending.pop(idx)
        return found

    def flush_ttl(self):
        """Purge ephemeral values that have expired"""
        if self.kvmap:
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

    def send_hugz(self):
        """Send hugz to anyone listening on the publisher socket"""
        kvmsg = KVMsg(self.sequence)
        kvmsg.key = "HUGZ"
        kvmsg.body = ""
        kvmsg.send(self.publisher)

    # ---------------------------------------------------------------------
    # State change handlers

    def become_master(self):
        """We're becoming master

        The backup server applies its pending list to its own hash table,
        and then starts to process state snapshot requests.
        """
        self.master = True
        self.slave = False
        # stop receiving subscriber updates while we are master
        self.subscriber.stop_on_recv()

        # Apply pending list to own kvmap
        while self.pending:
            kvmsg = self.pending.pop(0)
            self.sequence += 1
            kvmsg.sequence = self.sequence
            kvmsg.store(self.kvmap)
            logging.info ("I: publishing pending=%d", self.sequence)


    def become_slave(self):
        """We're becoming slave"""
        # clear kvmap
        self.kvmap = None
        self.master = False
        self.slave = True
        self.subscriber.on_recv(self.handle_subscriber)

    def handle_subscriber(self, msg):
        """Collect updates from peer (master)
        We're always slave when we get these updates
        """
        if self.master:
            logging.warn("received subscriber message, but we are master %s", msg)
            return

        # Get state snapshot if necessary
        if self.kvmap is None:
            self.kvmap = {}
            snapshot = self.ctx.socket(zmq.DEALER)
            snapshot.linger = 0
            snapshot.connect("tcp://localhost:%i" % self.peer)

            logging.info ("I: asking for snapshot from: tcp://localhost:%d",
                        self.peer)
            snapshot.send_multipart(["ICANHAZ?", ''])
            while True:
                try:
                    kvmsg = KVMsg.recv(snapshot)
                except KeyboardInterrupt:
                    # Interrupted
                    self.bstar.loop.stop()
                    return
                if kvmsg.key == "KTHXBAI":
                    self.sequence = kvmsg.sequence
                    break          # Done
                kvmsg.store(self.kvmap)

            logging.info ("I: received snapshot=%d", self.sequence)

        # Find and remove update off pending list
        kvmsg = KVMsg.from_msg(msg)
        # update float ttl -> timestamp
        ttl = float(kvmsg.get('ttl', 0))
        if ttl:
            kvmsg['ttl'] = time.time() + ttl

        if kvmsg.key != "HUGZ":
            if not self.was_pending(kvmsg):
                # If master update came before client update, flip it
                # around, store master update (with sequence) on pending
                # list and use to clear client update when it comes later
                self.pending.append(kvmsg)

            # If update is more recent than our kvmap, apply it
            if (kvmsg.sequence > self.sequence):
                self.sequence = kvmsg.sequence
                kvmsg.store(self.kvmap)
                logging.info ("I: received update=%d", self.sequence)


def main():
    import sys
    if '-p' in sys.argv:
        primary = True
    elif '-b' in sys.argv:
        primary = False
    else:
        print "Usage: clonesrv6.py { -p | -b }"
        sys.exit(1)
    clone = CloneServer(primary)
    clone.start()

if __name__ == '__main__':
    main()
