--
--  Task worker
--  Connects PULL socket to tcp://localhost:5557
--  Collects workloads from ventilator via that socket
--  Connects PUSH socket to tcp://localhost:5558
--  Sends results to sink via that socket
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"

local context = zmq.init(1)

--  Socket to receive messages on
local receiver = context:socket(zmq.PULL)
receiver:connect("tcp://localhost:5557")

--  Socket to send messages to
local sender = context:socket(zmq.PUSH)
sender:connect("tcp://localhost:5558")

--  Process tasks forever
while true do
    local msg = receiver:recv()
    --  Simple progress indicator for the viewer
    io.stdout:flush()
    printf("%s.", msg)

    --  Do the work
    s_sleep(tonumber(msg))

    --  Send results to sink
    sender:send("")
end
receiver:close()
sender:close()
context:term()


