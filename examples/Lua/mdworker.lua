--
--  Majordomo Protocol worker example
--  Uses the mdwrk API to hide all MDP aspects
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--

require"mdwrkapi"
require"zmsg"

local verbose = (arg[1] == "-v")
local session = mdwrkapi.new("tcp://localhost:5555", "echo", verbose)

local reply
while true do
    local request = session:recv(reply)
    if not request then
        break              --  Worker was interrupted
    end
    reply = request        --  Echo is complex... :-)
end
session:destroy()


