--
--  Simple request-reply broker
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zmq.poller"
require"zhelpers"

--  Prepare our context and sockets
local context = zmq.init(1)
local frontend = context:socket(zmq.ROUTER)
local backend  = context:socket(zmq.DEALER)
frontend:bind("tcp://*:5559")
backend:bind("tcp://*:5560")

--  Switch messages between sockets
local poller = zmq.poller(2)
poller:add(frontend, zmq.POLLIN, function()
    while true do
        --  Process all parts of the message
        local msg = frontend:recv()
        if (frontend:getopt(zmq.RCVMORE) == 1) then
            backend:send(msg, zmq.SNDMORE)
        else
            backend:send(msg, 0)
            break;      --  Last message part
        end
    end
end)
poller:add(backend, zmq.POLLIN, function()
    while true do
        --  Process all parts of the message
        local msg = backend:recv()
        if (backend:getopt(zmq.RCVMORE) == 1) then
            frontend:send(msg, zmq.SNDMORE)
        else
            frontend:send(msg, 0)
            break;      --  Last message part
        end
    end
end)

-- start poller's event loop
poller:start()

--  We never get here but clean up anyhow
frontend:close()
backend:close()
context:term()


