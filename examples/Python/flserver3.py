"""Freelance server - Model 3

Uses an ROUTER/ROUTER socket but just one thread

Author: Min RK <benjaminrk@gmail.com>
"""

import sys

import zmq

from zhelpers import dump

def main():
    verbose = '-v' in sys.argv
    
    ctx = zmq.Context()
    # Prepare server socket with predictable identity
    bind_endpoint = "tcp://*:5555"
    connect_endpoint = "tcp://localhost:5555"
    server = ctx.socket(zmq.ROUTER)
    server.identity = connect_endpoint
    server.bind(bind_endpoint)
    print "I: service is ready at", bind_endpoint
    
    while True:
        try:
            request = server.recv_multipart()
        except:
            break # Interrupted
        # Frame 0: identity of client
        # Frame 1: PING, or client control frame
        # Frame 2: request body
        address, control = request[:2]
        reply = [address, control]
        if control == "PING":
            reply[1] = "PONG"
        else:
            reply.append("OK")
        if verbose:
            dump(reply)
        server.send_multipart(reply)
    print "W: interrupted"

if __name__ == '__main__':
    main()