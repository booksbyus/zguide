--
--  Hello World server
--  Connects REP socket to tcp://*:5560
--  Expects "Hello" from client, replies with "World"
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"

local context = zmq.init(1)

--  Socket to talk to clients
local responder = context:socket(zmq.REP)
responder:connect("tcp://localhost:5560")

while true do
    --  Wait for next request from client
    local msg = responder:recv()
    printf ("Received request: [%s]\n", msg)

    --  Do some 'work'
    s_sleep (1000)

    --  Send reply back to client
    responder:send("World")
end
--  We never get here but clean up anyhow
responder:close()
context:term()


