--
--  Durable subscriber
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"

local context = zmq.init(1)

--  Connect our subscriber socket
local subscriber = context:socket(zmq.SUB)
subscriber:setopt(zmq.IDENTITY, "Hello")
subscriber:setopt(zmq.SUBSCRIBE, "")
subscriber:connect("tcp://localhost:5565")

--  Synchronize with publisher
local sync = context:socket(zmq.PUSH)
sync:connect("tcp://localhost:5564")
sync:send("")

--  Get updates, expect random Ctrl-C death
while true do
    local msg = subscriber:recv()
    printf("%s\n", msg)
    if (msg == "END") then
        break
    end
end
subscriber:close()
context:term()


