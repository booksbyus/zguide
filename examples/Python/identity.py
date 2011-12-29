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

sink = context.socket(zmq.ROUTER)
sink.bind("inproc://example")

# First allow 0MQ to set the identity
anonymous = context.socket(zmq.DEALER)
anonymous.connect("inproc://example")
anonymous.send("ROUTER uses a generated UUID")
zhelpers.dump(sink)

# Then set the identity ourself
identified = context.socket(zmq.DEALER)
identified.setsockopt(zmq.IDENTITY, "Hello")
identified.connect("inproc://example")
identified.send("ROUTER socket uses REQ's socket identity")
zhelpers.dump(sink)
