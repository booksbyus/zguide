#
# Freelance server - Model 2
# Does some work, replies OK, with message sequencing
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

print "I: Service is ready at %s" % endpoint
while True:
    request = server.recv_multipart()
    if not request:
        break  # Interrupted
    # Fail nastily if run against wrong client
    assert len(request) == 2

    address = request[0]
    reply = [address, "OK"]
    server.send_multipart(reply)

server.setsockopt(zmq.LINGER, 0)  # Terminate early
