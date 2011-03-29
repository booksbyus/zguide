--
--  Hello World client
--  Connects REQ socket to tcp://localhost:5559
--  Sends "Hello" to server, expects "World" back
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"

local context = zmq.init(1)

--  Socket to talk to server
local requester = context:socket(zmq.REQ)
requester:connect("tcp://localhost:5559")

for n=0,9 do
    requester:send("Hello")
    local msg = requester:recv()
    printf ("Received reply %d [%s]\n", n, msg)
end
requester:close()
context:term()


