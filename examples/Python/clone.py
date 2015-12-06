"""
clone - client-side Clone Pattern class

Author: Min RK <benjaminrk@gmail.com>
"""

import logging
import threading
import time

import zmq

from zhelpers import zpipe
from kvmsg import KVMsg

# If no server replies within this time, abandon request
GLOBAL_TIMEOUT  =   4000    # msecs
# Server considered dead if silent for this long
SERVER_TTL      =   5.0     # secs
# Number of servers we will talk to
SERVER_MAX      =   2

# basic log formatting:
logging.basicConfig(format="%(asctime)s %(message)s", datefmt="%Y-%m-%d %H:%M:%S",
        level=logging.INFO)

# =====================================================================
# Synchronous part, works in our application thread

class Clone(object):
    ctx = None      # Our Context
    pipe = None     # Pipe through to clone agent
    agent = None    # agent in a thread
    _subtree = None # cache of our subtree value

    def __init__(self):
        self.ctx = zmq.Context()
        self.pipe, peer = zpipe(self.ctx)
        self.agent = threading.Thread(target=clone_agent, args=(self.ctx,peer))
        self.agent.daemon = True
        self.agent.start()

    # ---------------------------------------------------------------------
    # Clone.subtree is a property, which sets the subtree for snapshot
    # and updates

    @property
    def subtree(self):
        return self._subtree

    @subtree.setter
    def subtree(self, subtree):
        """Sends [SUBTREE][subtree] to the agent"""
        self._subtree = subtree
        self.pipe.send_multipart(["SUBTREE", subtree])

    def connect(self, address, port):
        """Connect to new server endpoint
        Sends [CONNECT][address][port] to the agent
        """
        self.pipe.send_multipart(["CONNECT", address, str(port)])

    def set(self, key, value, ttl=0):
        """Set new value in distributed hash table
        Sends [SET][key][value][ttl] to the agent
        """
        self.pipe.send_multipart(["SET", key, value, str(ttl)])

    def get(self, key):
        """Lookup value in distributed hash table
        Sends [GET][key] to the agent and waits for a value response
        If there is no clone available, will eventually return None.
        """

        self.pipe.send_multipart(["GET", key])
        try:
            reply = self.pipe.recv_multipart()
        except KeyboardInterrupt:
            return
        else:
            return reply[0]


# =====================================================================
# Asynchronous part, works in the background

# ---------------------------------------------------------------------
# Simple class for one server we talk to

class CloneServer(object):
    address = None          # Server address
    port = None             # Server port
    snapshot = None         # Snapshot socket
    subscriber = None       # Incoming updates
    expiry = 0              # Expires at this time
    requests = 0            # How many snapshot requests made?

    def __init__(self, ctx, address, port, subtree):
        self.address = address
        self.port = port
        self.snapshot = ctx.socket(zmq.DEALER)
        self.snapshot.linger = 0
        self.snapshot.connect("%s:%i" % (address,port))
        self.subscriber = ctx.socket(zmq.SUB)
        self.subscriber.setsockopt(zmq.SUBSCRIBE, subtree)
        self.subscriber.setsockopt(zmq.SUBSCRIBE, b'HUGZ')
        self.subscriber.connect("%s:%i" % (address,port+1))
        self.subscriber.linger = 0


# ---------------------------------------------------------------------
# Simple class for one background agent

#  States we can be in
STATE_INITIAL   =    0   #  Before asking server for state
STATE_SYNCING   =    1   #  Getting state from server
STATE_ACTIVE    =    2   #  Getting new updates from server

class CloneAgent(object):
    ctx = None              # Own context
    pipe = None             # Socket to talk back to application
    kvmap = None            # Actual key/value dict
    subtree = ''            # Subtree specification, if any
    servers = None          # list of connected Servers
    state = 0               # Current state
    cur_server = 0          # If active, index of server in list
    sequence = 0            # last kvmsg procesed
    publisher = None        # Outgoing updates

    def __init__(self, ctx, pipe):
        self.ctx = ctx
        self.pipe = pipe
        self.kvmap = {}
        self.subtree = ''
        self.state = STATE_INITIAL
        self.publisher = ctx.socket(zmq.PUB)
        self.router = ctx.socket(zmq.ROUTER)
        self.servers = []

    def control_message (self):
        msg = self.pipe.recv_multipart()
        command = msg.pop(0)

        if command == "CONNECT":
            address = msg.pop(0)
            port = int(msg.pop(0))
            if len(self.servers) < SERVER_MAX:
                self.servers.append(CloneServer(self.ctx, address, port, self.subtree))
                self.publisher.connect("%s:%i" % (address,port+2))
            else:
                logging.error("E: too many servers (max. %i)", SERVER_MAX)
        elif command == "SET":
            key,value,sttl = msg
            ttl = int(sttl)

            # Send key-value pair on to server
            kvmsg = KVMsg(0, key=key, body=value)
            kvmsg.store(self.kvmap)
            if ttl:
                kvmsg["ttl"] = ttl
            kvmsg.send(self.publisher)
        elif command == "GET":
            key = msg[0]
            value = self.kvmap.get(key)
            self.pipe.send(value.body if value else '')
        elif command == "SUBTREE":
            self.subtree = msg[0]


# ---------------------------------------------------------------------
# Asynchronous agent manages server pool and handles request/reply
# dialog when the application asks for it.

def clone_agent(ctx, pipe):
    agent = CloneAgent(ctx, pipe)
    server = None

    while True:
        poller = zmq.Poller()
        poller.register(agent.pipe, zmq.POLLIN)
        poll_timer = None
        server_socket = None

        if agent.state == STATE_INITIAL:
            # In this state we ask the server for a snapshot,
            # if we have a server to talk to...
            if agent.servers:
                server = agent.servers[agent.cur_server]
                logging.info ("I: waiting for server at %s:%d...",
                    server.address, server.port)
                if (server.requests < 2):
                    server.snapshot.send_multipart(["ICANHAZ?", agent.subtree])
                    server.requests += 1
                server.expiry = time.time() + SERVER_TTL
                agent.state = STATE_SYNCING
                server_socket = server.snapshot
        elif agent.state == STATE_SYNCING:
            # In this state we read from snapshot and we expect
            # the server to respond, else we fail over.
            server_socket = server.snapshot
        elif agent.state == STATE_ACTIVE:
            # In this state we read from subscriber and we expect
            # the server to give hugz, else we fail over.
            server_socket = server.subscriber

        if server_socket:
            # we have a second socket to poll:
            poller.register(server_socket, zmq.POLLIN)

        if server is not None:
            poll_timer = 1e3 * max(0,server.expiry - time.time())

        # ------------------------------------------------------------
        # Poll loop
        try:
            items = dict(poller.poll(poll_timer))
        except:
            raise # DEBUG
            break # Context has been shut down

        if agent.pipe in items:
            agent.control_message()
        elif server_socket in items:
            kvmsg = KVMsg.recv(server_socket)

            # Anything from server resets its expiry time
            server.expiry = time.time() + SERVER_TTL
            if (agent.state == STATE_SYNCING):
                # Store in snapshot until we're finished
                server.requests = 0
                if kvmsg.key == "KTHXBAI":
                    agent.sequence = kvmsg.sequence
                    agent.state = STATE_ACTIVE
                    logging.info ("I: received from %s:%d snapshot=%d",
                        server.address, server.port, agent.sequence)
                else:
                    kvmsg.store(agent.kvmap)
            elif (agent.state == STATE_ACTIVE):
                # Discard out-of-sequence updates, incl. hugz
                if (kvmsg.sequence > agent.sequence):
                    agent.sequence = kvmsg.sequence
                    kvmsg.store(agent.kvmap)
                    action = "update" if kvmsg.body else "delete"

                    logging.info ("I: received from %s:%d %s=%d",
                        server.address, server.port, action, agent.sequence)
        else:
            # Server has died, failover to next
            logging.info ("I: server at %s:%d didn't give hugz",
                    server.address, server.port)
            agent.cur_server = (agent.cur_server + 1) % len(agent.servers)
            agent.state = STATE_INITIAL
