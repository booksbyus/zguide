--
--  Reading from multiple sockets
--  This version uses :poll()
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zmq.poller"
require"zhelpers"

local context = zmq.init(1)

--  Connect to task ventilator
local receiver = context:socket(zmq.PULL)
receiver:connect("tcp://localhost:5557")

--  Connect to weather server
local subscriber = context:socket(zmq.SUB)
subscriber:connect("tcp://localhost:5556")
subscriber:setopt(zmq.SUBSCRIBE, "10001 ", 6)

local poller = zmq.poller(2)

poller:add(receiver, zmq.POLLIN, function()
    local msg = receiver:recv()
    --  Process task
end)

poller:add(subscriber, zmq.POLLIN, function()
    local msg = subscriber:recv()
    --  Process weather update
end)

--  Process messages from both sockets
-- start poller's event loop
poller:start()

--  We never get here
receiver:close()
subscriber:close()
context:term()


