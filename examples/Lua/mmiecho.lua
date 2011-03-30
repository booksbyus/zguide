--
--  MMI echo query example
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--

require"mdcliapi"
require"zmsg"
require"zhelpers"

local verbose = (arg[1] == "-v")
local session = mdcliapi.new("tcp://localhost:5555", verbose)

--  This is the service we want to look up
local request = zmsg.new("echo")

--  This is the service we send our request to
local reply = session:send("mmi.service", request)

if (reply) then
    printf ("Lookup echo service: %s\n", reply:body())
else
    printf ("E: no response from broker, make sure it's running\n")
end

session:destroy()


