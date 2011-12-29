--
--  Demonstrate identities as used by the request-reply pattern.  Run this
--  program by itself.  Note that the utility functions s_ are provided by
--  zhelpers.h.  It gets boring for everyone to keep repeating this code.
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"

local context = zmq.init(1)

local sink = context:socket(zmq.ROUTER)
sink:bind("inproc://example")

--  First allow 0MQ to set the identity
local anonymous = context:socket(zmq.REQ)
anonymous:connect("inproc://example")
anonymous:send("ROUTER uses a generated UUID")
s_dump(sink)

--  Then set the identity ourself
local identified = context:socket(zmq.REQ)
identified:setopt(zmq.IDENTITY, "Hello")
identified:connect("inproc://example")
identified:send("ROUTER socket uses REQ's socket identity")
s_dump(sink)

sink:close()
anonymous:close()
identified:close()
context:term()


