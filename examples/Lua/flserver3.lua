--
--  Freelance server - Model 3
--  Uses an ROUTER/ROUTER socket but just one thread
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zmsg"

local verbose = (arg[1] == "-v")

local context = zmq.init(1)
s_catch_signals ()

--  Prepare server socket with predictable identity
local bind_endpoint = "tcp://*:5555"
local connect_endpoint = "tcp://localhost:5555"
local server = context:socket(zmq.ROUTER)
server:setopt(zmq.IDENTITY, connect_endpoint)
server:bind(bind_endpoint)
printf ("I: service is ready at %s\n", bind_endpoint)

while (not s_interrupted) do
    local request = zmsg.recv (server)
    local reply = nil
    if (not request) then
        break          --  Interrupted
    end
    if (verbose) then
        request:dump()
    end
    --  Frame 0: identity of client
    --  Frame 1: PING, or client control frame
    --  Frame 2: request body
    local address = request:pop()
    if (request:parts() == 1 and request:body() == "PING") then
        reply = zmsg.new ("PONG")
    elseif (request:parts() > 1) then
        reply = request
        request = nil
        reply:body_set("OK")
    end
    reply:push(address)
    if (verbose and reply) then
        reply:dump()
    end
    reply:send(server)
end
if (s_interrupted) then
    printf ("W: interrupted\n")
end
server:close()
context:term()


