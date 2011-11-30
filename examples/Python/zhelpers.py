# encoding: utf-8
"""
Helper module for example applications. Mimics ZeroMQ Guide's zhelpers.h.
"""

import binascii

from random import randint

import zmq

# fix ROUTER/DEALER aliases, missing from pyzmq < 2.1.9
if not hasattr(zmq, 'ROUTER'):
    zmq.ROUTER = zmq.XREP
if not hasattr(zmq, 'DEALER'):
    zmq.DEALER = zmq.XREQ


# Receives all message parts from socket, prints neatly
def dump(zsocket):
    print "----------------------------------------"
    for part in zsocket.recv_multipart():
        print "[%03d]" % len(part),
        try:
            # print only if ascii
            print part.decode()
        except:
            # not ascii, print hex
            print binascii.hexlify(part)


# Set simple random printable identity on socket
def set_id(zsocket):
    identity = "%04x-%04x" % (randint(0, 0x10000), randint(0, 0x10000))
    zsocket.setsockopt(zmq.IDENTITY, identity)
