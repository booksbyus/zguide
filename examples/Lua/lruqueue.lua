--
--  Least-recently used (LRU) queue device
--  Clients and workers are shown here in-process
--
--  While this example runs in a single process, that is just to make
--  it easier to start and stop the example. Each thread has its own
--  context and conceptually acts as a separate process.
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--

require"zmq"
require"zmq.threads"
require"zmq.poller"
require"zhelpers"

local tremove = table.remove

local NBR_CLIENTS  = 10
local NBR_WORKERS  = 3

local pre_code = [[
    local identity, seed = ...
    local zmq = require"zmq"
    require"zhelpers"
    math.randomseed(seed)
]]

--  Basic request-reply client using REQ socket
--  Since s_send and s_recv can't handle 0MQ binary identities we
--  set a printable text identity to allow routing.
--
local client_task = pre_code .. [[
    local context = zmq.init(1)
    local client = context:socket(zmq.REQ)
    client:setopt(zmq.IDENTITY, identity)  --  Set a printable identity
    client:connect("ipc://frontend.ipc")

    --  Send request, get reply
    client:send("HELLO")
    local reply = client:recv()
    printf ("Client: %s\n", reply)

    client:close()
    context:term()
]]

--  Worker using REQ socket to do LRU routing
--  Since s_send and s_recv can't handle 0MQ binary identities we
--  set a printable text identity to allow routing.
--
local worker_task = pre_code .. [[
    local context = zmq.init(1)
    local worker = context:socket(zmq.REQ)
    worker:setopt(zmq.IDENTITY, identity)  --  Set a printable identity
    worker:connect("ipc://backend.ipc")

    --  Tell broker we're ready for work
    worker:send("READY")

    while true do
        --  Read and save all frames until we get an empty frame
        --  In this example there is only 1 but it could be more
        local address = worker:recv()
        local empty = worker:recv()
        assert (#empty == 0)

        --  Get request, send reply
        local request = worker:recv()
        printf ("Worker: %s\n", request)

        worker:send(address, zmq.SNDMORE)
        worker:send("", zmq.SNDMORE)
        worker:send("OK")

    end
    worker:close()
    context:term()
]]

s_version_assert (2, 1)

--  Prepare our context and sockets
local context = zmq.init(1)
local frontend = context:socket(zmq.ROUTER)
local backend  = context:socket(zmq.ROUTER)
frontend:bind("ipc://frontend.ipc")
backend:bind("ipc://backend.ipc")

local clients = {}
for n=1,NBR_CLIENTS do
    local identity = string.format("%04X-%04X", randof (0x10000), randof (0x10000))
    local seed = os.time() + math.random()
    clients[n] = zmq.threads.runstring(context, client_task, identity, seed)
    clients[n]:start()
end
local workers = {}
for n=1,NBR_WORKERS do
    local identity = string.format("%04X-%04X", randof (0x10000), randof (0x10000))
    local seed = os.time() + math.random()
    workers[n] = zmq.threads.runstring(context, worker_task, identity, seed)
    workers[n]:start(true)
end

--  Logic of LRU loop
--  - Poll backend always, frontend only if 1+ worker ready
--  - If worker replies, queue worker as ready and forward reply
--    to client if necessary
--  - If client requests, pop next worker and send request to it

--  Queue of available workers
local worker_queue = {}

local is_accepting = false
local max_requests = #clients

local poller = zmq.poller(2)

local function frontend_cb()
    --  Now get next client request, route to LRU worker
    --  Client request is [address][empty][request]
    local client_addr = frontend:recv()
    local empty = frontend:recv()
    assert (#empty == 0)

    local request = frontend:recv()

    -- Dequeue a worker from the queue.
    local worker = tremove(worker_queue, 1)

    backend:send(worker, zmq.SNDMORE)
    backend:send("", zmq.SNDMORE)
    backend:send(client_addr, zmq.SNDMORE)
    backend:send("", zmq.SNDMORE)
    backend:send(request)

    if (#worker_queue == 0) then
        -- stop accepting work from clients, when no workers are available.
        poller:remove(frontend)
        is_accepting = false
    end
end

poller:add(backend, zmq.POLLIN, function()
    --  Queue worker address for LRU routing
    local worker_addr = backend:recv()
    worker_queue[#worker_queue + 1] = worker_addr

    -- start accepting client requests, if we are not already doing so.
    if not is_accepting then
        is_accepting = true
        poller:add(frontend, zmq.POLLIN, frontend_cb)
    end

    --  Second frame is empty
    local empty = backend:recv()
    assert (#empty == 0)

    --  Third frame is READY or else a client reply address
    local client_addr = backend:recv()

    --  If client reply, send rest back to frontend
    if (client_addr ~= "READY") then
        empty = backend:recv()
        assert (#empty == 0)

        local reply = backend:recv()
        frontend:send(client_addr, zmq.SNDMORE)
        frontend:send("", zmq.SNDMORE)
        frontend:send(reply)

        max_requests = max_requests - 1
        if (max_requests == 0) then
            poller:stop()      --  Exit after N messages
        end
    end
end)

-- start poller's event loop
poller:start()

frontend:close()
backend:close()
context:term()

for n=1,NBR_CLIENTS do
    assert(clients[n]:join())
end
-- workers are detached, we don't need to join with them.

