--
--  Pubsub envelope publisher
--  Note that the zhelpers.h file also provides s_sendmore
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"

--  Prepare our context and publisher
local context = zmq.init(1)
local publisher = context:socket(zmq.PUB)
publisher:bind("tcp://*:5563")

while true do
    --  Write two messages, each with an envelope and content
    publisher:send("A", zmq.SNDMORE)
    publisher:send("We don't want to see this")
    publisher:send("B", zmq.SNDMORE)
    publisher:send("We would like to see this")
    s_sleep (1000)
end
--  We never get here but clean up anyhow
publisher:close()
context:term()


