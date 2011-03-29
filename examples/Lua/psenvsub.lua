--
--  Pubsub envelope subscriber
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"

--  Prepare our context and subscriber
local context = zmq.init(1)
local subscriber = context:socket(zmq.SUB)
subscriber:connect("tcp://localhost:5563")
subscriber:setopt(zmq.SUBSCRIBE, "B")

while true do
    --  Read envelope with address
    local address = subscriber:recv()
    --  Read message contents
    local contents = subscriber:recv()
    printf("[%s] %s\n", address, contents)

end
--  We never get here but clean up anyhow
subscriber:close()
context:term()


