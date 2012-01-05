#
# Freelance Client - Model 2
# Uses DEALER socket to blast one or more services
#
# Author: Daniel Lundin <dln(at)eintr(dot)org>
#

import sys
import time

import zmq

GLOBAL_TIMEOUT = 2500  # ms

class FLClient(object):
    def __init__(self):
        self.servers = 0
        self.sequence = 0
        self.context = zmq.Context()
        self.socket = self.context.socket(zmq.DEALER)   # DEALER

    def destroy(self):
        self.socket.setsockopt(zmq.LINGER, 0)  # Terminate early
        self.socket.close()
        self.context.term()

    def connect(self, endpoint):
        self.socket.connect(endpoint)
        self.servers += 1
        print "I: Connected to %s" % endpoint

    def request(self, *request):
        # Prefix request with sequence number and empty envelope
        self.sequence += 1
        msg = ['', str(self.sequence)] + list(request)


        # Blast the request to all connected servers
        for server in xrange(self.servers):
            self.socket.send_multipart(msg)

        # Wait for a matching reply to arrive from anywhere
        # Since we can poll several times, calculate each one
        poll = zmq.Poller()
        poll.register(self.socket, zmq.POLLIN)

        reply = None
        endtime = time.time() + GLOBAL_TIMEOUT / 1000
        while time.time() < endtime:
            socks = dict(poll.poll((endtime - time.time()) * 1000))
            if socks.get(self.socket) == zmq.POLLIN:
                reply = self.socket.recv_multipart()
                assert len(reply) == 3
                sequence = int(reply[1])
                if sequence == self.sequence:
                    break
        return reply

if len(sys.argv) == 1:
    print "I: Usage: %s <endpoint> ..." % sys.argv[0]
    sys.exit(0)

# Create new freelance client object
client = FLClient()

for endpoint in sys.argv[1:]:
    client.connect(endpoint)

start = time.time()
for requests in xrange(10000):
    request = "random name"
    reply = client.request(request)
    if not reply:
        print "E: Name service not available, aborting"
        break
print "Average round trip cost: %i usec" % ((time.time() - start) / 100)
client.destroy()

