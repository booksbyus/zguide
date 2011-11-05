# encoding: utf-8
"""
Helper module for example applications. Mimics ZeroMQ Guide's zhelpers.h.
"""

from random import randint

import zmq

pyzmq_version = tuple(map(int, zmq.pyzmq_version().split('.')))
if pyzmq_version <= (2, 1, 7):
    zmq.ROUTER = zmq.XREP


# Receives all message parts from socket, prints neatly
def dump(zsocket):
    print "----------------------------------------"
    for part in zsocket.recv_multipart():
        print "[%03d]" % len(part),
        if all(31 < ord(c) < 128 for c in part):
            print part
        else:
            print "".join("%x" % ord(c) for c in part)


# Set simple random printable identity on socket
def set_id(zsocket):
    identity = "%04x-%04x" % (randint(0, 0x10000), randint(0, 0x10000))
    zsocket.setsockopt(zmq.IDENTITY, identity)
