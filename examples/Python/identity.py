# encoding: utf-8
#
#   Demonstrate identities as used by the request-reply pattern.  Run this
#   program by itself.
#
#   Author: Jeremy Avnet (brainsik) <spork(dash)zmq(at)theory(dot)org>
#

import zmq
import zhelpers

context = zmq.Context()

sink = context.socket(zmq.XREP)
sink.bind("inproc://example")

# First allow 0MQ to set the identity
anonymous = context.socket(zmq.XREQ)
anonymous.connect("inproc://example")
anonymous.send("XREP uses a generated UUID")
zhelpers.dump(sink)

# Then set the identity ourself
identified = context.socket(zmq.XREQ)
identified.setsockopt(zmq.IDENTITY, "Hello")
identified.connect("inproc://example")
identified.send("XREP socket uses REQ's socket identity")
zhelpers.dump(sink)
