--
--  Shows how to handle Ctrl-C
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"

local context = zmq.init(1)
local server = context:socket(zmq.REP)
server:bind("tcp://*:5555")

s_catch_signals ()
while true do
    --  Blocking read will exit on a signal
    local request = server:recv()
    if (s_interrupted) then
        printf ("W: interrupt received, killing server...\n")
        break
    end
    server:send("World")
end
server:close()
context:term()


