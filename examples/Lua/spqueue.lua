--
--  Simple Pirate queue
--  This is identical to the LRU pattern, with no reliability mechanisms
--  at all. It depends on the client for recovery. Runs forever.
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--

require"zmq"
require"zmq.poller"
require"zhelpers"
require"zmsg"

local tremove = table.remove

local MAX_WORKERS  = 100

s_version_assert (2, 1)

--  Prepare our context and sockets
local context = zmq.init(1)
local frontend = context:socket(zmq.ROUTER)
local backend  = context:socket(zmq.ROUTER)
frontend:bind("tcp://*:5555");    --  For clients
backend:bind("tcp://*:5556");    --  For workers

--  Queue of available workers
local worker_queue = {}
local is_accepting = false

local poller = zmq.poller(2)

local function frontend_cb()
    --  Now get next client request, route to next worker
    local msg = zmsg.recv (frontend)

    -- Dequeue a worker from the queue.
    local worker = tremove(worker_queue, 1)

    msg:wrap(worker, "")
    msg:send(backend)

    if (#worker_queue == 0) then
        -- stop accepting work from clients, when no workers are available.
        poller:remove(frontend)
        is_accepting = false
    end
end

--  Handle worker activity on backend
poller:add(backend, zmq.POLLIN, function()
    local msg = zmsg.recv(backend)
    --  Use worker address for LRU routing
    worker_queue[#worker_queue + 1] = msg:unwrap()

    -- start accepting client requests, if we are not already doing so.
    if not is_accepting then
        is_accepting = true
        poller:add(frontend, zmq.POLLIN, frontend_cb)
    end

    --  Forward message to client if it's not a READY
    if (msg:address() ~= "READY") then
        msg:send(frontend)
    end
end)

-- start poller's event loop
poller:start()

--  We never exit the main loop

