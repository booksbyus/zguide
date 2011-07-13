#
# Freelance server - Model 1
# Trivial echo service
#
# Author: Daniel Lundin <dln(at)eintr(dot)org>
#

import sys
import zmq

if len(sys.argv) < 2:
    print "I: Syntax: %s <endpoint>" % sys.argv[0]
    sys.exit(0)

endpoint = sys.argv[1]
context = zmq.Context()
server = context.socket(zmq.REP)
server.bind(endpoint)

print "I: Echo service is ready at %s" % endpoint
while True:
    msg = server.recv_multipart()
    if not msg:
        break  # Interrupted
    server.send_multipart(msg)

server.setsockopt(zmq.LINGER, 0) # Terminate immediately
