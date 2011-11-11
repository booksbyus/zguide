--
--  Publisher for durable subscriber
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"

s_version_assert(2, 1)

local context = zmq.init(1)

--  Subscriber tells us when it's ready here
local sync = context:socket(zmq.PULL)
sync:bind("tcp://*:5564")

--  We send updates via this socket
local publisher = context:socket(zmq.PUB)

--  Prevent publisher overflow from slow subscribers
publisher:setopt(zmq.HWM, 1)

--  Specify swap space in bytes, this covers all subscribers
publisher:setopt(zmq.SWAP, 25000000)
publisher:bind("tcp://*:5565")

--  Wait for synchronization request
local message = sync:recv()

--  Now broadcast exactly 10 updates with pause
for n=0,9 do
    local message = string.format("Update %d", n)
    publisher:send(message)
    s_sleep (1000)
end
publisher:send("END")

sync:close()
publisher:close()
context:term()

