--
--  Hello World client
--  Connects REQ socket to tcp://localhost:5555
--  Sends "Hello" to server, expects "World" back
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"

local context = zmq.init(1)

--  Socket to talk to server
print("Connecting to hello world server...")
local socket = context:socket(zmq.REQ)
socket:connect("tcp://localhost:5555")

for n=1,10 do
    print("Sending Hello " .. n .. " ...")
    socket:send("Hello")

    local reply = socket:recv()
    print("Received World " ..  n .. " [" .. reply .. "]")
end
socket:close()
context:term()

