"""
flcliapi - Freelance Pattern agent class
Model 3: uses ROUTER socket to address specific services

Author: Min RK <benjaminrk@gmail.com>
"""

import threading
import time

import zmq

from zhelpers import zpipe

# If no server replies within this time, abandon request
GLOBAL_TIMEOUT = 3000    # msecs
# PING interval for servers we think are alivecp 
PING_INTERVAL  = 2000    # msecs
# Server considered dead if silent for this long
SERVER_TTL     = 6000    # msecs


def flciapi_agent(peer):
    """This is the thread that handles our real flcliapi class
    """
    pass

# =====================================================================
# Synchronous part, works in our application thread

class FreelanceClient(object):
    ctx = None      # Our Context
    pipe = None     # Pipe through to flciapi agent
    agent = None    # agent in a thread
    
    def __init__(self):
        self.ctx = zmq.Context()
        self.pipe, peer = zpipe(self.ctx)
        self.agent = threading.Thread(target=agent_task, args=(self.ctx,peer))
        self.agent.daemon = True
        self.agent.start()
        
    
    def connect(self, endpoint):
        """Connect to new server endpoint
        Sends [CONNECT][endpoint] to the agent
        """
        self.pipe.send_multipart(["CONNECT", endpoint])
        time.sleep(0.1) # Allow connection to come up

    def request(self, msg):
        "Send request, get reply"
        request = ["REQUEST"] + msg
        self.pipe.send_multipart(request)
        reply = self.pipe.recv_multipart()
        status = reply.pop(0)
        if status != "FAILED":
            return reply


# =====================================================================
# Asynchronous part, works in the background

# ---------------------------------------------------------------------
# Simple class for one server we talk to

class FreelanceServer(object):
    endpoint = None         # Server identity/endpoint
    alive = True            # 1 if known to be alive
    ping_at = 0             # Next ping at this time
    expires = 0             # Expires at this time

    def __init__(self, endpoint):
        self.endpoint = endpoint
        self.alive = True
        self.ping_at = time.time() + 1e-3*PING_INTERVAL
        self.expires = time.time() + 1e-3*SERVER_TTL
    
    def ping(self, socket):
        if time.time() > self.ping_at:
            socket.send_multipart([self.endpoint, 'PING'])
            self.ping_at = time.time() + 1e-3*PING_INTERVAL
    
    def tickless(self, tickless):
        if tickless > self.ping_at:
            tickless = self.ping_at
        return tickless

# ---------------------------------------------------------------------
# Simple class for one background agent

class FreelanceAgent(object):
    ctx = None              # Own context
    pipe = None             # Socket to talk back to application
    router = None           # Socket to talk to servers
    servers = None          # Servers we've connected to
    actives = None          # Servers we know are alive
    sequence = 0            # Number of requests ever sent
    request = None          # Current request if any
    reply = None            # Current reply if any
    expires = 0             # Timeout for request/reply

    def __init__(self, ctx, pipe):
        self.ctx = ctx
        self.pipe = pipe
        self.router = ctx.socket(zmq.ROUTER)
        self.servers = {}
        self.actives = []

    def control_message (self):
        msg = self.pipe.recv_multipart()
        command = msg.pop(0)

        if command == "CONNECT":
            endpoint = msg.pop(0)
            print "I: connecting to %s...\n" % endpoint,
            self.router.connect(endpoint)
            server = FreelanceServer(endpoint)
            self.servers[endpoint] = server
            self.actives.append(server)
            # these are in the C case, but seem redundant:
            server.ping_at = time.time() + 1e-3*PING_INTERVAL
            server.expires = time.time() + 1e-3*SERVER_TTL
        elif command == "REQUEST":
            assert not self.request    # Strict request-reply cycle
            # Prefix request with sequence number and empty envelope
            self.request = [str(self.sequence), ''] + msg
        
            # Request expires after global timeout
            self.expires = time.time() + 1e-3*GLOBAL_TIMEOUT

    def router_message (self):
        reply = self.router.recv_multipart()
        # Frame 0 is server that replied
        endpoint = reply.pop(0)
        server = self.servers[endpoint]
        if not server.alive:
            self.actives.append(server)
            server.alive = 1
    
        server.ping_at = time.time() + 1e-3*PING_INTERVAL
        server.expires = time.time() + 1e-3*SERVER_TTL;

        # Frame 1 may be sequence number for reply
        sequence = reply.pop(0)
        if int(sequence) == self.sequence:
            self.sequence += 1
            reply = ["OK"] + reply
            self.pipe.send_multipart(reply)
            self.request = None


# ---------------------------------------------------------------------
# Asynchronous agent manages server pool and handles request/reply
# dialog when the application asks for it.

def agent_task(ctx, pipe):
    agent = FreelanceAgent(ctx, pipe)
    poller = zmq.Poller()
    poller.register(agent.pipe, zmq.POLLIN)
    poller.register(agent.router, zmq.POLLIN)
    
    while True:
        # Calculate tickless timer, up to 1 hour
        tickless = time.time() + 3600
        if (agent.request and tickless > agent.expires):
            tickless = agent.expires
            for server in agent.servers.values():
                tickless = server.tickless(tickless)
        try:
            items = dict(poller.poll(1000 * (tickless - time.time())))
        except:
            break              # Context has been shut down

        if agent.pipe in items:
            agent.control_message()

        if agent.router in items:
            agent.router_message()

        # If we're processing a request, dispatch to next server
        if (agent.request):
            if (time.time() >= agent.expires):
                # Request expired, kill it
                agent.pipe.send("FAILED")
                agent.request = None
            else:
                # Find server to talk to, remove any expired ones
                while agent.actives:
                    server = agent.actives[0]
                    if time.time() >= server.expires:
                        server.alive = 0
                        agent.actives.pop(0)
                    else:
                        request = [server.endpoint] + agent.request
                        agent.router.send_multipart(request)
                        break

        # Disconnect and delete any expired servers
        # Send heartbeats to idle servers if needed
        for server in agent.servers.values():
            server.ping(agent.router)
