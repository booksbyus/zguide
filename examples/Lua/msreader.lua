--
--  Reading from multiple sockets
--  This version uses a simple recv loop
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"

--  Prepare our context and sockets
local context = zmq.init(1)

--  Connect to task ventilator
local receiver = context:socket(zmq.PULL)
receiver:connect("tcp://localhost:5557")

--  Connect to weather server
local subscriber = context:socket(zmq.SUB)
subscriber:connect("tcp://localhost:5556")
subscriber:setopt(zmq.SUBSCRIBE, "10001 ")

--  Process messages from both sockets
--  We prioritize traffic from the task ventilator
while true do
    --  Process any waiting tasks
    local msg
    while true do
        msg = receiver:recv(zmq.NOBLOCK)
        if not msg then break end
        --  process task
    end
    --  Process any waiting weather updates
    while true do
        msg = subscriber:recv(zmq.NOBLOCK)
        if not msg then break end
        --  process weather update
    end
    --  No activity, so sleep for 1 msec
    s_sleep (1)
end
--  We never get here but clean up anyhow
receiver:close()
subscriber:close()
context:term()


