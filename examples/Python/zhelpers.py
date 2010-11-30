# encoding: utf-8
"""
Helper module for example applications. Mimics ZeroMQ Guide's zhelpers.h.
"""

def dump(zsocket):
    print "----------------------------------------"
    for part in zsocket.recv_multipart():
        print "[%03d]" % len(part),
        if all(31 < ord(c) < 128 for c in part):
            print part
        else:
            print "".join("%x" % ord(c) for c in part)
