--
--  Multithreaded Hello World server
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zmq.threads"
require"zhelpers"

local worker_code = [[
    local id = ...

    local zmq = require"zmq"
    require"zhelpers"
    local threads = require"zmq.threads"
    local context = threads.get_parent_ctx()

    --  Socket to talk to dispatcher
    local receiver = context:socket(zmq.REP)
    assert(receiver:connect("inproc://workers"))

    while true do
        local msg = receiver:recv()
        printf ("Received request: [%s]\n", msg)

        --  Do some 'work'
        s_sleep (1000)

        --  Send reply back to client
        receiver:send("World")
    end
    receiver:close()
    return nil
]]

s_version_assert (2, 1)
local context = zmq.init(1)

--  Socket to talk to clients
local clients = context:socket(zmq.ROUTER)
clients:bind("tcp://*:5555")

--  Socket to talk to workers
local workers = context:socket(zmq.DEALER)
workers:bind("inproc://workers")

--  Launch pool of worker threads
local worker_pool = {}
for n=1,5 do
    worker_pool[n] = zmq.threads.runstring(context, worker_code, n)
    worker_pool[n]:start()
end
--  Connect work threads to client threads via a queue
print("start queue device.")
zmq.device(zmq.QUEUE, clients, workers)

--  We never get here but clean up anyhow
clients:close()
workers:close()
context:term()


