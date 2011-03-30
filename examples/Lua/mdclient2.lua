--
--  Majordomo Protocol client example - asynchronous
--  Uses the mdcli API to hide all MDP aspects
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--

require"mdcliapi2"
require"zmsg"
require"zhelpers"

local verbose = (arg[1] == "-v")
local session = mdcliapi2.new("tcp://localhost:5555", verbose)

local count=100000
for n=1,count do
    local request = zmsg.new("Hello world")
    session:send("echo", request)
end
for n=1,count do
    local reply = session:recv()
    if not reply then
        break   --  Interrupted by Ctrl-C
    end
end
printf("%d replies received\n", count)
session:destroy()

