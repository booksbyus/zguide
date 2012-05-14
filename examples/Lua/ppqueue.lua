--
--  Paranoid Pirate queue
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zmq.poller"
require"zmsg"

local MAX_WORKERS          = 100
local HEARTBEAT_LIVENESS   = 3       --  3-5 is reasonable
local HEARTBEAT_INTERVAL   = 1000    --  msecs

local tremove = table.remove

--  Insert worker at end of queue, reset expiry
--  Worker must not already be in queue
local function s_worker_append(queue, identity)
    if queue[identity] then
        printf ("E: duplicate worker identity %s", identity)
    else
        assert (#queue < MAX_WORKERS)
        queue[identity] = s_clock() + HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS
        queue[#queue + 1] = identity
    end
end
--  Remove worker from queue, if present
local function s_worker_delete(queue, identity)
    for i=1,#queue do
        if queue[i] == identity then
            tremove(queue, i)
            break
        end
    end
    queue[identity] = nil
end
--  Reset worker expiry, worker must be present
local function s_worker_refresh(queue, identity)
    if queue[identity] then
        queue[identity] = s_clock() + HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS
    else
        printf("E: worker %s not ready\n", identity)
    end
end
--  Pop next available worker off queue, return identity
local function s_worker_dequeue(queue)
    assert (#queue > 0)
    local identity = tremove(queue, 1)
    queue[identity] = nil
    return identity
end
--  Look for & kill expired workers
local function s_queue_purge(queue)
    local curr_clock = s_clock()
    --  Work backwards from end to simplify removal
    for i=#queue,1,-1 do
        local id = queue[i]
        if (curr_clock > queue[id]) then
            tremove(queue, i)
            queue[id] = nil
        end
    end
end
s_version_assert (2, 1)

--  Prepare our context and sockets
local context = zmq.init(1)
local frontend = context:socket(zmq.ROUTER)
local backend  = context:socket(zmq.ROUTER)
frontend:bind("tcp://*:5555");    --  For clients
backend:bind("tcp://*:5556");    --  For workers

--  Queue of available workers
local queue = {}
local is_accepting = false

--  Send out heartbeats at regular intervals
local heartbeat_at = s_clock() + HEARTBEAT_INTERVAL

local poller = zmq.poller(2)

local function frontend_cb()
    --  Now get next client request, route to next worker
    local msg = zmsg.recv(frontend)
    local identity = s_worker_dequeue (queue)
    msg:push(identity)
    msg:send(backend)

    if (#queue == 0) then
        -- stop accepting work from clients, when no workers are available.
        poller:remove(frontend)
        is_accepting = false
    end
end

--  Handle worker activity on backend
poller:add(backend, zmq.POLLIN, function()
    local msg = zmsg.recv(backend)
    local identity = msg:unwrap()

    --  Return reply to client if it's not a control message
    if (msg:parts() == 1) then
        if (msg:address() == "READY") then
            s_worker_delete(queue, identity)
            s_worker_append(queue, identity)
        elseif (msg:address() == "HEARTBEAT") then
            s_worker_refresh(queue, identity)
        else
            printf("E: invalid message from %s\n", identity)
            msg:dump()
        end
    else
        -- reply for client.
        msg:send(frontend)
        s_worker_append(queue, identity)
    end

    -- start accepting client requests, if we are not already doing so.
    if not is_accepting and #queue > 0 then
        is_accepting = true
        poller:add(frontend, zmq.POLLIN, frontend_cb)
    end
end)

-- start poller's event loop
while true do
    local cnt = assert(poller:poll(HEARTBEAT_INTERVAL * 1000))
    --  Send heartbeats to idle workers if it's time
    if (s_clock() > heartbeat_at) then
        for i=1,#queue do
            local msg = zmsg.new("HEARTBEAT")
            msg:wrap(queue[i], nil)
            msg:send(backend)
        end
        heartbeat_at = s_clock() + HEARTBEAT_INTERVAL
    end
    s_queue_purge(queue)
end

--  We never exit the main loop
--  But pretend to do the right shutdown anyhow
while (#queue > 0) do
    s_worker_dequeue(queue)
end

frontend:close()
backend:close()


