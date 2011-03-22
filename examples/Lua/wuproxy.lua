--
--  Weather proxy device
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"

local context = zmq.init(1)

--  This is where the weather server sits
local frontend = context:socket(zmq.SUB)
frontend:connect(arg[1] or "tcp://192.168.55.210:5556")

--  This is our public endpolocal for subscribers
local backend = context:socket(zmq.PUB)
backend:bind(arg[2] or "tcp://10.1.1.0:8100")

--  Subscribe on everything
frontend:setopt(zmq.SUBSCRIBE, "")

--  Shunt messages out to our own subscribers
while true do
    while true do
        --  Process all parts of the message
        local message = frontend:recv()
        if frontend:getopt(zmq.RCVMORE) == 1 then
            backend:send(message, zmq.SNDMORE)
        else
            backend:send(message)
            break      --  Last message part
        end
    end
end
--  We don't actually get here but if we did, we'd shut down neatly
frontend:close()
backend:close()
context:term()
