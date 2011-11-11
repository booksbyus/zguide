--
--  Hello World server
--  Binds REP socket to tcp://*:5555
--  Expects "Hello" from client, replies with "World"
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"

local context = zmq.init(1)

--  Socket to talk to clients
local socket = context:socket(zmq.REP)
socket:bind("tcp://*:5555")

while true do
    --  Wait for next request from client
    local request = socket:recv()
    print("Received Hello [" .. request .. "]")

    --  Do some 'work'
		s_sleep(1000)

    --  Send reply back to client
    socket:send("World")
end
--  We never get here but if we did, this would be how we end
socket:close()
context:term()
