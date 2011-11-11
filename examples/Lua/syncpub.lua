--
--  Synchronized publisher
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"

--  We wait for 10 subscribers
SUBSCRIBERS_EXPECTED   = 10

s_version_assert (2, 1)
local context = zmq.init(1)

--  Socket to talk to clients
local publisher = context:socket(zmq.PUB)
publisher:bind("tcp://*:5561")

--  Socket to receive signals
local syncservice = context:socket(zmq.REP)
syncservice:bind("tcp://*:5562")

--  Get synchronization from subscribers
local subscribers = 0
while (subscribers < SUBSCRIBERS_EXPECTED) do
    --  - wait for synchronization request
    local msg = syncservice:recv()

    --  - send synchronization reply
    syncservice:send("")
    subscribers = subscribers + 1
end
--  Now broadcast exactly 1M updates followed by END
local update_nbr
for update_nbr=1,1000000 do
    publisher:send("Rhubarb")
end

publisher:send("END")

publisher:close()
syncservice:close()
context:term()


