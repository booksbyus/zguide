--
--  Synchronized subscriber
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"

local context = zmq.init(1)

--  First, connect our subscriber socket
local subscriber = context:socket(zmq.SUB)
subscriber:connect("tcp://localhost:5561")
subscriber:setopt(zmq.SUBSCRIBE, "")

--  0MQ is so fast, we need to wait a while...
s_sleep (1000)

--  Second, synchronize with publisher
local syncclient = context:socket(zmq.REQ)
syncclient:connect("tcp://localhost:5562")

--  - send a synchronization request
syncclient:send("")

--  - wait for synchronization reply
local msg = syncclient:recv()

--  Third, get our updates and report how many we got
local update_nbr = 0
while true do
    local msg = subscriber:recv()
    if (msg == "END") then
        break
    end
    update_nbr = update_nbr + 1
end
printf ("Received %d updates\n", update_nbr)

subscriber:close()
syncclient:close()
context:term()


