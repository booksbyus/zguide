--
--  Task worker - design 2
--  Adds pub-sub flow to receive and respond to kill signal
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zmq.poller"
require"zhelpers"

local context = zmq.init(1)

--  Socket to receive messages on
local receiver = context:socket(zmq.PULL)
receiver:connect("tcp://localhost:5557")

--  Socket to send messages to
local sender = context:socket(zmq.PUSH)
sender:connect("tcp://localhost:5558")

--  Socket for control input
local controller = context:socket(zmq.SUB)
controller:connect("tcp://localhost:5559")
controller:setopt(zmq.SUBSCRIBE, "", 0)

--  Process messages from receiver and controller
local poller = zmq.poller(2)
poller:add(receiver, zmq.POLLIN, function()
    local msg = receiver:recv()

    --  Do the work
    s_sleep(tonumber(msg))

    --  Send results to sink
    sender:send("")

    --  Simple progress indicator for the viewer
    io.write(".")
    io.stdout:flush()
end)
poller:add(controller, zmq.POLLIN, function()
    poller:stop() --  Exit loop
end)

-- start poller's event loop
poller:start()

--  Finished
receiver:close()
sender:close()
controller:close()
context:term()


