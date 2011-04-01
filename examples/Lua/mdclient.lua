--
--  Majordomo Protocol client example
--  Uses the mdcli API to hide all MDP aspects
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--

require"mdcliapi"
require"zmsg"
require"zhelpers"

local verbose = (arg[1] == "-v")
local session = mdcliapi.new("tcp://localhost:5555", verbose)

local count=1
repeat
    local request = zmsg.new("Hello world")
    local reply = session:send("echo", request)
    if not reply then
        break    --  Interrupt or failure
    end
    count = count + 1
until (count == 100000)
printf("%d requests/replies processed\n", count)
session:destroy()

