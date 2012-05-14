--
--  Broker peering simulation (part 3)
--  Prototypes the full flow of status and tasks
--
--  While this example runs in a single process, that is just to make
--  it easier to start and stop the example. Each thread has its own
--  context and conceptually acts as a separate process.
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--

require"zmq"
require"zmq.poller"
require"zmq.threads"
require"zmsg"

local tremove = table.remove

local NBR_CLIENTS  = 10
local NBR_WORKERS  = 5

local pre_code = [[
    local self, seed = ...
    local zmq = require"zmq"
    local zmsg = require"zmsg"
    require"zhelpers"
    math.randomseed(seed)
    local context = zmq.init(1)

]]

--  Request-reply client using REQ socket
--  To simulate load, clients issue a burst of requests and then
--  sleep for a random period.
--
local client_task = pre_code .. [[
    require"zmq.poller"

    local client = context:socket(zmq.REQ)
    local endpoint = string.format("ipc://%s-localfe.ipc", self)
    assert(client:connect(endpoint))

    local monitor = context:socket(zmq.PUSH)
    local endpoint = string.format("ipc://%s-monitor.ipc", self)
    assert(monitor:connect(endpoint))

    local poller = zmq.poller(1)
    local task_id = nil

    poller:add(client, zmq.POLLIN, function()
        local msg = zmsg.recv (client)
        --  Worker is supposed to answer us with our task id
        assert (msg:body() == task_id)
        -- mark task as processed.
        task_id = nil
    end)
    local is_running = true
    while is_running do
        s_sleep (randof (5) * 1000)

        local burst = randof (15)
        while (burst > 0) do
            burst = burst - 1
            --  Send request with random hex ID
            task_id = string.format("%04X", randof (0x10000))
            local msg = zmsg.new(task_id)
            msg:send(client)

            --  Wait max ten seconds for a reply, then complain
            rc = poller:poll(10 * 1000000)
            assert (rc >= 0)

            if task_id then
                local msg = zmsg.new()
                msg:body_fmt(
                    "E: CLIENT EXIT - lost task %s", task_id)
                msg:send(monitor)
                -- exit event loop
                is_running = false
                break
            end
        end
    end
    --  We never get here but if we did, this is how we'd exit cleanly
    client:close()
    monitor:close()
    context:term()
]]

--  Worker using REQ socket to do LRU routing
--
local worker_task = pre_code .. [[
    local worker = context:socket(zmq.REQ)
    local endpoint = string.format("ipc://%s-localbe.ipc", self)
    assert(worker:connect(endpoint))

    --  Tell broker we're ready for work
    local msg = zmsg.new ("READY")
    msg:send(worker)

    while true do
        --  Workers are busy for 0/1/2 seconds
        msg = zmsg.recv (worker)
        s_sleep (randof (2) * 1000)
        msg:send(worker)
    end
    --  We never get here but if we did, this is how we'd exit cleanly
    worker:close()
    context:term()
]]

--  First argument is this broker's name
--  Other arguments are our peers' names
--
s_version_assert (2, 1)
if (#arg < 1) then
    printf ("syntax: peering3 me doyouend...\n")
    os.exit(-1)
end
--  Our own name; in practice this'd be configured per node
local self = arg[1]
printf ("I: preparing broker at %s...\n", self)
math.randomseed(os.time())

--  Prepare our context and sockets
local context = zmq.init(1)

--  Bind cloud frontend to endpoint
local cloudfe = context:socket(zmq.ROUTER)
local endpoint = string.format("ipc://%s-cloud.ipc", self)
cloudfe:setopt(zmq.IDENTITY, self)
assert(cloudfe:bind(endpoint))

--  Bind state backend / publisher to endpoint
local statebe = context:socket(zmq.PUB)
local endpoint = string.format("ipc://%s-state.ipc", self)
assert(statebe:bind(endpoint))

--  Connect cloud backend to all peers
local cloudbe = context:socket(zmq.ROUTER)
cloudbe:setopt(zmq.IDENTITY, self)

for n=2,#arg do
    local peer = arg[n]
    printf ("I: connecting to cloud frontend at '%s'\n", peer)
    local endpoint = string.format("ipc://%s-cloud.ipc", peer)
    assert(cloudbe:connect(endpoint))
end
--  Connect statefe to all peers
local statefe = context:socket(zmq.SUB)
statefe:setopt(zmq.SUBSCRIBE, "", 0)

local peers = {}
for n=2,#arg do
    local peer = arg[n]
    -- add peer name to peers list.
    peers[#peers + 1] = peer
    peers[peer] = 0 -- set peer's initial capacity to zero.
    printf ("I: connecting to state backend at '%s'\n", peer)
    local endpoint = string.format("ipc://%s-state.ipc", peer)
    assert(statefe:connect(endpoint))
end
--  Prepare local frontend and backend
local localfe = context:socket(zmq.ROUTER)
local endpoint = string.format("ipc://%s-localfe.ipc", self)
assert(localfe:bind(endpoint))

local localbe = context:socket(zmq.ROUTER)
local endpoint = string.format("ipc://%s-localbe.ipc", self)
assert(localbe:bind(endpoint))

--  Prepare monitor socket
local monitor = context:socket(zmq.PULL)
local endpoint = string.format("ipc://%s-monitor.ipc", self)
assert(monitor:bind(endpoint))

--  Start local workers
local workers = {}
for n=1,NBR_WORKERS do
    local seed = os.time() + math.random()
    workers[n] = zmq.threads.runstring(nil, worker_task, self, seed)
    workers[n]:start(true)
end
--  Start local clients
local clients = {}
for n=1,NBR_CLIENTS do
    local seed = os.time() + math.random()
    clients[n] = zmq.threads.runstring(nil, client_task, self, seed)
    clients[n]:start(true)
end

--  Interesting part
--  -------------------------------------------------------------
--  Publish-subscribe flow
--  - Poll statefe and process capacity updates
--  - Each time capacity changes, broadcast new value
--  Request-reply flow
--  - Poll primary and process local/cloud replies
--  - While worker available, route localfe to local or cloud

--  Queue of available workers
local local_capacity = 0
local cloud_capacity = 0
local worker_queue = {}
local backends = zmq.poller(2)

local function send_reply(msg)
    local address = msg:address()
    -- Route reply to cloud if it's addressed to a broker
    if peers[address] then
        msg:send(cloudfe) -- reply is for a peer.
    else
        msg:send(localfe) -- reply is for a local client.
    end
end

backends:add(localbe, zmq.POLLIN, function()
    local msg = zmsg.recv(localbe)

    --  Use worker address for LRU routing
    local_capacity = local_capacity + 1
    worker_queue[local_capacity] = msg:unwrap()
    -- if reply is not "READY" then route reply back to client.
    if (msg:address() ~= "READY") then
        send_reply(msg)
    end
end)

backends:add(cloudbe, zmq.POLLIN, function()
    local msg = zmsg.recv(cloudbe)

    --  We don't use peer broker address for anything
    msg:unwrap()
    -- send reply back to client.
    send_reply(msg)
end)

backends:add(statefe, zmq.POLLIN, function()
    local msg = zmsg.recv (statefe)
    -- TODO: track capacity for each peer
    cloud_capacity = tonumber(msg:body())
end)

backends:add(monitor, zmq.POLLIN, function()
    local msg = zmsg.recv (monitor)
    printf("%s\n", msg:body())
end)

local frontends = zmq.poller(2)
local localfe_ready = false
local cloudfe_ready = false

frontends:add(localfe, zmq.POLLIN, function() localfe_ready = true end)
frontends:add(cloudfe, zmq.POLLIN, function() cloudfe_ready = true end)

local MAX_BACKEND_REPLIES = 20

while true do
    -- If we have no workers anyhow, wait indefinitely
    local timeout = (local_capacity > 0) and 1000000 or -1
    local rc, err = backends:poll(timeout)
    assert (rc >= 0, err)

    --  Track if capacity changes during this iteration
    local previous = local_capacity

    --  Now route as many clients requests as we can handle
    --  - If we have local capacity we poll both localfe and cloudfe
    --  - If we have cloud capacity only, we poll just localfe
    --  - Route any request locally if we can, else to cloud
    --
    while ((local_capacity + cloud_capacity) > 0) do
        local rc, err = frontends:poll(0)
        assert (rc >= 0, err)

        if (localfe_ready) then
            localfe_ready = false
            msg = zmsg.recv (localfe)
        elseif (cloudfe_ready and local_capacity > 0) then
            cloudfe_ready = false
            -- we have local capacity poll cloud frontend for work.
            msg = zmsg.recv (cloudfe)
        else
            break;      --  No work, go back to primary
        end

        if (local_capacity > 0) then
            --  Dequeue and drop the next worker address
            local worker = tremove(worker_queue, 1)
            local_capacity = local_capacity - 1
            msg:wrap(worker, "")
            msg:send(localbe)
        else
            --  Route to random broker peer
            printf ("I: route request %s to cloud...\n",
                msg:body())
            local random_peer = randof (#peers) + 1
            msg:wrap(peers[random_peer], nil)
            msg:send(cloudbe)
        end
    end
    if (local_capacity ~= previous) then
        --  Broadcast new capacity
        local msg = zmsg.new()
        -- TODO: send our name with capacity.
        msg:body_fmt("%d", local_capacity)
        --  We stick our own address onto the envelope
        msg:wrap(self, nil)
        msg:send(statebe)
    end
end
--  We never get here but clean up anyhow
localbe:close()
cloudbe:close()
localfe:close()
cloudfe:close()
statefe:close()
monitor:close()
context:term()


