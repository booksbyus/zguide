--
--  Asynchronous client-to-server (DEALER to ROUTER)
--
--  While this example runs in a single process, that is just to make
--  it easier to start and stop the example. Each task has its own
--  context and conceptually acts as a separate process.
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--

require"zmq"
require"zmq.threads"
require"zmsg"
require"zhelpers"

local NBR_CLIENTS  = 3

--  ---------------------------------------------------------------------
--  This is our client task
--  It connects to the server, and then sends a request once per second
--  It collects responses as they arrive, and it prints them out. We will
--  run several client tasks in parallel, each with a different random ID.

local client_task = [[
    local identity, seed = ...
    local zmq = require"zmq"
    require"zmq.poller"
    require"zmq.threads"
    local zmsg = require"zmsg"
    require"zhelpers"
    math.randomseed(seed)

    local context = zmq.init(1)
    local client = context:socket(zmq.DEALER)

    --  Generate printable identity for the client
    client:setopt(zmq.IDENTITY, identity)
    client:connect("tcp://localhost:5570")

    local poller = zmq.poller(2)

    poller:add(client, zmq.POLLIN, function()
        local msg = zmsg.recv (client)
        printf ("%s: %s\n", identity, msg:body())
    end)
    local request_nbr = 0
    while true do
        --  Tick once per second, pulling in arriving messages
        local centitick
        for centitick=1,100 do
            poller:poll(10000)
        end
        local msg = zmsg.new()
        request_nbr = request_nbr + 1
        msg:body_fmt("request #%d", request_nbr)
        msg:send(client)
    end
    --  Clean up and end task properly
    client:close()
    context:term()
]]

--  ---------------------------------------------------------------------
--  This is our server task
--  It uses the multithreaded server model to deal requests out to a pool
--  of workers and route replies back to clients. One worker can handle
--  one request at a time but one client can talk to multiple workers at
--  once.

local server_task = [[
    local server_worker = ...
    local zmq = require"zmq"
    require"zmq.poller"
    require"zmq.threads"
    local zmsg = require"zmsg"
    require"zhelpers"
    math.randomseed(os.time())

    local context = zmq.init(1)

    --  Frontend socket talks to clients over TCP
    local frontend = context:socket(zmq.ROUTER)
    frontend:bind("tcp://*:5570")

    --  Backend socket talks to workers over inproc
    local backend = context:socket(zmq.DEALER)
    backend:bind("inproc://backend")

    --  Launch pool of worker threads, precise number is not critical
    local workers = {}
    for n=1,5 do
        local seed = os.time() + math.random()
        workers[n] = zmq.threads.runstring(context, server_worker, seed)
        workers[n]:start()
    end
    --  Connect backend to frontend via a queue device
    --  We could do this:
    --      zmq:device(.QUEUE, frontend, backend)
    --  But doing it ourselves means we can debug this more easily

    local poller = zmq.poller(2)

    poller:add(frontend, zmq.POLLIN, function()
        local msg = zmsg.recv (frontend)
        --print ("Request from client:")
        --msg:dump()
        msg:send(backend)
    end)
    poller:add(backend, zmq.POLLIN, function()
        local msg = zmsg.recv (backend)
        --print ("Reply from worker:")
        --msg:dump()
        msg:send(frontend)
    end)
    --  Switch messages between frontend and backend
    poller:start()

    for n=1,5 do
        assert(workers[n]:join())
    end
    frontend:close()
    backend:close()
    context:term()
]]

--  Accept a request and reply with the same text a random number of
--  times, with random delays between replies.
--
local server_worker = [[
    local seed = ...
    local zmq = require"zmq"
    require"zmq.threads"
    local zmsg = require"zmsg"
    require"zhelpers"
    math.randomseed(seed)
    local threads = require"zmq.threads"
    local context = threads.get_parent_ctx()

    local worker = context:socket(zmq.DEALER)
    worker:connect("inproc://backend")

    while true do
        --  The DEALER socket gives us the address envelope and message
        local msg = zmsg.recv (worker)
        assert (msg:parts() == 2)

        --  Send 0..4 replies back
        local reply
        local replies = randof (5)
        for reply=1,replies do
            --  Sleep for some fraction of a second
            s_sleep (randof (1000) + 1)
            local dup = msg:dup()
            dup:send(worker)
        end
    end
    worker:close()
]]

--  This main thread simply starts several clients, and a server, and then
--  waits for the server to finish.
--

s_version_assert (2, 1)

local clients = {}
for n=1,NBR_CLIENTS do
    local identity = string.format("%04X", randof (0x10000))
    local seed = os.time() + math.random()
    clients[n] = zmq.threads.runstring(nil, client_task, identity, seed)
    clients[n]:start()
end

local server = zmq.threads.runstring(nil, server_task, server_worker)
assert(server:start())
assert(server:join())

